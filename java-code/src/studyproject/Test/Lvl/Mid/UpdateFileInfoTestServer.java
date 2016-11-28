package studyproject.Test.Lvl.Mid;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
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
				BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
			){
			in.readLine();
			BufferedOutputStream out = new BufferedOutputStream(socket.getOutputStream());
			String[] parts = message.split("\n");
			for(String line: parts){
				line += "\n";
				out.write(line.getBytes());
				out.flush();
			}
		} catch(IOException e){
			System.err.println(e.getStackTrace());
		}
	}

}
