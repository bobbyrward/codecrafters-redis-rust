#![allow(dead_code)]
#![allow(unused_imports)]
mod client;
mod engine;
mod error;
mod resp;
mod server;

use std::sync::Arc;
use std::error::Error;
use crate::engine::Cache;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error + Send + Sync + 'static>> {
    let cache = Arc::new(crate::engine::Cache::new());

    eprintln!("Server starting");
    crate::server::serve("127.0.0.1:6379", cache).await?;
    eprintln!("Server shutting down");
    Ok(())
}
