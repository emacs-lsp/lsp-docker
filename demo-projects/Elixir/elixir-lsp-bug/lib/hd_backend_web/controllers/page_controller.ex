defmodule HdBackendWeb.PageController do
  use HdBackendWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
