package studyproject.Test.Lvl.Mid;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;

public class UpdateFileInfoTestServer extends Thread{
	
	private int port;
	private InetAddress ip;
	private String message;

	public UpdateFileInfoTestServer(InetAddress ip, int port, String message){
		this.ip = ip;
		this.port = port;
		this.message = message;
	}

	@Override
	public void run() {
		try(ServerSocket serverSocket = new ServerSocket(port, 0, ip);
				Socket socket = serverSocket.accept();
				BufferedOutputStream out = new BufferedOutputStream(socket.getOutputStream())){
			out.write(message.getBytes());
		} catch(IOException e){
			System.err.println(e.getStackTrace());
		}
	}

}
