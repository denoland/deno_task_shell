use tokio::io::AsyncWrite;

pub trait Environment: Clone + Send + Sync + 'static {
  fn async_stdout(&self) -> Box<dyn AsyncWrite + Unpin + Send + Sync>;
  fn eprintln(&self, text: &str);
}

#[derive(Clone, Default)]
pub struct RealEnvironment {}

impl Environment for RealEnvironment {
  fn async_stdout(&self) -> Box<dyn AsyncWrite + Unpin + Send + Sync> {
    Box::new(tokio::io::stdout())
  }

  fn eprintln(&self, text: &str) {
    eprintln!("{}", text);
  }
}
