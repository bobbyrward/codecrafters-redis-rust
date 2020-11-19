use tokio::sync::{mpsc, oneshot};

use crate::error::EngineError;

const ENGINE_COMMAND_QUEUE_SIZE: usize = 64;

pub(crate) type PingResponseSender = oneshot::Sender<()>;
pub(crate) type PingResponseReceiver = oneshot::Receiver<()>;

pub(crate) type EngineCommandSender = mpsc::Sender<EngineCommand>;
pub(crate) type EngineCommandReceiver = mpsc::Receiver<EngineCommand>;

pub(crate) enum EngineCommand {
    Ping { responder: PingResponseSender },
    Shutdown,
}

pub(crate) struct Engine {
    command_receiver: EngineCommandReceiver,
    command_sender: EngineCommandSender,
}

impl Engine {
    pub(crate) async fn new() -> Self {
        let (tx, rx) = mpsc::channel(ENGINE_COMMAND_QUEUE_SIZE);

        Self {
            command_receiver: rx,
            command_sender: tx,
        }
    }

    pub(crate) fn event_sender(&self) -> &EngineCommandSender {
        &self.command_sender
    }

    pub(crate) async fn process(&mut self) -> Result<(), EngineError> {
        Ok(())
    }
}
