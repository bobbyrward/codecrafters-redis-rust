#![allow(dead_code)]
#![allow(unused_imports)]
mod client;
mod engine;
mod error;
mod resp;
mod server;

use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error + Send + Sync + 'static>> {
    crate::server::serve("127.0.0.1:6379").await?;
    Ok(())
}
