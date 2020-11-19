use bytes::BytesMut;
use std::net::SocketAddr;
use tokio::net::TcpStream;
use tokio::prelude::*;

use crate::error::EngineError;
use crate::resp::RespValue;

// async fn process_resp(
enum Command {
    CommandList,
    Ping,
}

pub(crate) struct Client {
    address: SocketAddr,
    socket: TcpStream,
}

impl Client {
    pub(crate) fn new(address: SocketAddr, socket: TcpStream) -> Self {
        Self { address, socket }
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

    pub(crate) async fn process(&mut self) -> Result<(), EngineError> {
        let future = async move {
            eprintln!("Processing connection from {:?}", self.address);

            let mut buffer = BytesMut::with_capacity(1024);

            loop {
                self.socket.read_buf(&mut buffer).await?;

                if buffer.len() == 0 {
                    return Ok(());
                }

                eprintln!("Bytes received; len={}, {:?}", buffer.len(), &buffer[..]);

                loop {
                    if let Some(command) = RespValue::from_buf(&mut buffer)? {
                        eprintln!("New command: {:?}", command);

                        if let RespValue::Array(values) = command {
                            if values.len() < 1 {
                                return Err(EngineError::unknown(&format!(
                                    "Client sent command without a command: {:?}",
                                    values
                                )));
                            }

                            let command_string = values[0].as_str()?;

                            eprintln!("Client sent command: {}", command_string);

                            match command_string {
                                "PING" => {
                                    eprintln!("PING");
                                    let response = RespValue::simple_string("PONG");
                                    self.socket.write_all(&response.encode()?).await?;
                                }
                                "COMMAND" => {
                                    eprintln!("COMMAND");
                                }
                                _ => {
                                    eprintln!("UNKNOWN");
                                }
                            }
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
