use std::sync::Arc;
use bytes::BytesMut;
use std::net::SocketAddr;
use std::collections::HashMap;
use tokio::net::TcpStream;
use tokio::prelude::*;

use crate::Cache;
use crate::error::EngineError;
use crate::resp::RespValue;

// async fn process_resp(
enum Command {
    CommandList,
    Ping,
}

#[derive(Debug)]
pub(crate) struct Client {
    address: SocketAddr,
    socket: TcpStream,
    cache: HashMap<String, Vec<u8>>,
}

impl Client {
    pub(crate) fn new(address: SocketAddr, socket: TcpStream) -> Self {
        Self { address, socket, cache: HashMap::new() }
    }

    /*
    async fn read_next(buffer: &mut BytesMut, socket: &mut TcpStream) -> Result<RespValue> {
        loop {
            if let Some(line) = take_crlf(&mut buffer) {
            }

            socket.read_buf(&mut buffer).await?;
        }
    }
    */

    async fn process_command(&mut self, command: &str, args: &[RespValue], cache: Arc<Cache>) -> Result<RespValue, EngineError> {
        eprintln!("Client sent command: {} ({:?})", command, args);

        match command {
            "PING" => {
                let response = match args.len() {
                    0 => RespValue::simple_string("PONG"),
                    1 => args[0].clone(),
                    _ => return Err(EngineError::unknown(&format!("Client sent weird ping command: {:?}", args))),
                };

                eprintln!("Responding with: {:?}", response);

                Ok(response)
            }
            "ECHO" => {
                let response = match args.len() {
                    1 => args[0].clone(),
                    _ => return Err(EngineError::unknown(&format!("Client sent bad command: {} {:?}", command, args))),
                };

                eprintln!("Responding with: {:?}", response);

                Ok(response)
            }
            "SET" => {
                let response = match args.len() {
                    2 => {
                        cache.set_str(args[0].as_str()?, args[1].clone()).await?
                    }
                    _ => return Err(EngineError::unknown(&format!("Client sent bad command: {} {:?}", command, args))),
                };

                eprintln!("Responding with: {:?}", response);

                Ok(response)
            }
            "GET" => {
                let response = match args.len() {
                    1 => {
                        cache.get_str(args[0].as_str()?).await?
                    }
                    _ => return Err(EngineError::unknown(&format!("Client sent bad command: {} {:?}", command, args))),
                };

                eprintln!("Responding with: {:?}", response);

                Ok(response)
            }
            "COMMAND" => {
                eprintln!("COMMAND");
                Ok(RespValue::Array(vec![]))
            }
            _ => {
                eprintln!("UNKNOWN");
                Ok(RespValue::Array(vec![]))
            }
        }
    }

    pub(crate) async fn process(&mut self, cache: Arc<Cache>) -> Result<(), EngineError> {
        let future = async move {
            eprintln!("Processing connection from {:?}", self.address);

            let mut buffer = BytesMut::with_capacity(1024);

            loop {
                self.socket.read_buf(&mut buffer).await?;

                if buffer.len() == 0 {
                    return Ok(());
                }

                loop {
                    if let Some(command) = RespValue::from_buf(&mut buffer)? {
                        if let RespValue::Array(values) = command {
                            if values.len() < 1 {
                                return Err(EngineError::unknown(&format!(
                                    "Client sent command without a command: {:?}",
                                    values
                                )));
                            }

                            let command_string = values[0].as_str()?.to_ascii_uppercase();
                            let response = self.process_command(&command_string, &values[1..], cache.clone()).await?;

                            self.socket.write_all(&response.encode()?).await?;
                        } else {
                            return Err(EngineError::unknown(&format!(
                                "Client sent command that isn't an array: {:?}",
                                command
                            )));
                        }
                    } else {
                        break;
                    }
                }
            }
        };

        let result = future.await;

        eprintln!("Client disconnected: {:?}", result);

        result
    }
}
