package studyproject.Test.Lvl.Mid;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;

import studyproject.API.Lvl.Low.Responses;

public class SendFileWPTestClient extends Thread {

	private InetAddress ip;
	private int port;
	private String fileDestination;
	private long fileSize;

	public SendFileWPTestClient(InetAddress ip, int port, String fileDestination, long fileSize){
		this.ip = ip;
		this.port = port;
		this.fileDestination = fileDestination;
		this.fileSize = fileSize;
	}

	@Override
	public void run() {
		try(ServerSocket serverSocket = new ServerSocket(port, 0, ip);
				Socket socket = serverSocket.accept();
				FileOutputStream fileStream = new FileOutputStream(new File(fileDestination))){
			Responses.respondSendPermission(socket, fileStream, fileSize);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
