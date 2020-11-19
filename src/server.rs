use std::sync::Arc;

use tokio::sync::mpsc;

use crate::error::EngineError;
use crate::client::Client;
use crate::Cache;

const ACCEPTOR_QUEUE_SIZE: usize = 64;

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

pub(crate) async fn serve<A>(bind_address: A, cache: Arc<Cache>) -> Result<(), EngineError>
where
    A: tokio::net::ToSocketAddrs,
{
    let mut acceptor = listen(bind_address).await?;

    loop {
        tokio::select! {
            evt = acceptor.recv() => {
                if let Some(client) = evt {
                    let client_cache = cache.clone();

                    tokio::spawn(async move {
                        let mut client = client;

                        if let Err(err) = client.process(client_cache).await {
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

    Ok(())
}
