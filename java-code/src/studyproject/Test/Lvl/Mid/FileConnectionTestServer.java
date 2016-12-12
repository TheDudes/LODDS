package studyproject.Test.Lvl.Mid;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;

import studyproject.API.Lvl.Low.Responses;

public class FileConnectionTestServer extends Thread{

	private InetAddress ip;
	private int port;
	private String fileName;
	private long startIndex;
	private long endIndex;
	
	public FileConnectionTestServer(InetAddress ip, int port, String fileName, long startIndex, long endIndex){
		this.ip = ip;
		this.port = port;
		this.fileName = fileName;
		this.startIndex = startIndex;
		this.endIndex = endIndex;
	}
	
	@Override
	public void run() {
		try(ServerSocket servSocket = new ServerSocket(port, 0, ip);
				Socket socket = servSocket.accept();
				BufferedOutputStream socketStream = new BufferedOutputStream(socket.getOutputStream());
				FileInputStream fileStream = new FileInputStream(new File(fileName))){
			Responses.respondFile(socketStream, fileStream, startIndex, endIndex);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
