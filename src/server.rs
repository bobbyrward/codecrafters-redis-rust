use tokio::sync::mpsc;

use crate::error::EngineError;
//use crate::engine::{Engine, EngineCommand};
use crate::client::Client;

const ACCEPTOR_QUEUE_SIZE: usize = 64;

// type AcceptorSender = mpsc::Sender<Client>;
type AcceptorReceiver = mpsc::Receiver<Client>;

async fn listen<A>(bind_address: A) -> Result<AcceptorReceiver, EngineError>
where
    A: tokio::net::ToSocketAddrs,
{
    let mut listener = tokio::net::TcpListener::bind(bind_address).await?;
    let (mut tx, rx) = mpsc::channel(ACCEPTOR_QUEUE_SIZE);

    tokio::spawn(async move {
        loop {
            match listener.accept().await {
                Ok((socket, addr)) => {
                    println!("accepted new client: {:?}", addr);

                    // ignore the result
                    let _ = tx.send(Client::new(addr, socket)).await;
                }
                Err(e) => {
                    println!("couldn't accept client: {:?}", e);
                }
            }
        }
    });

    Ok(rx)
}

pub(crate) async fn serve<A>(bind_address: A) -> Result<(), EngineError>
where
    A: tokio::net::ToSocketAddrs,
{
    /*
    let mut engine = Engine::new().await;
    let command_sender = engine.event_sender().clone();

    let engine_task = tokio::spawn(async move {
        engine.process().await
    });
    */

    let mut acceptor = listen(bind_address).await?;

    loop {
        tokio::select! {
            evt = acceptor.recv() => {
                if let Some(client) = evt {
                    tokio::spawn(async move {
                        let mut client = client;

                        if let Err(err) = client.process().await {
                            eprintln!("Error from Client::process; {}", err);
                        }
                    });
                } else {
                    break;
                }
            }
            _ = tokio::signal::ctrl_c() => {
                break;
            }
        }
    }

    /*
    let send_result = command_sender.send_timeout(EngineCommand::Shutdown, tokio::time::Duration::from_secs(1)).await;

    if send_result.is_ok() {
        engine_task.await?;
    }
    */

    Ok(())
}
