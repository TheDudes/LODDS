package studyproject.Test.Lvl.Mid;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;

import studyproject.API.Lvl.Low.Responses;

/**
 * Dummy Client for the SendFileWPTest Junit tests
 * @author Michael
 *
 */
public class SendFileWPTestClient extends Thread {

	private InetAddress ip;
	private int port;
	private String fileDestination;
	private long fileSize;
	private long waitTime;

	public SendFileWPTestClient(InetAddress ip, int port,
			String fileDestination, long fileSize, long waitTime) {
		this.ip = ip;
		this.port = port;
		this.fileDestination = fileDestination;
		this.fileSize = fileSize;
		this.waitTime = waitTime;
	}

	@Override
	public void run() {
		try (ServerSocket serverSocket = new ServerSocket(port, 0, ip);
				Socket socket = serverSocket.accept();
				FileOutputStream fileStream = new FileOutputStream(new File(
						fileDestination))) {
			BufferedReader reader = new BufferedReader(new InputStreamReader(
					socket.getInputStream()));
			reader.readLine();
			Thread.sleep(waitTime);
			Responses.respondSendPermission(socket, fileStream, fileSize);
		} catch (IOException e) {
			e.printStackTrace();
		} catch (InterruptedException e) {
		}
	}

}
