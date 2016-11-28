package studyproject.Test.Lvl.Mid;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;

import studyproject.API.Core.Utils;

/**
 * Dummy Client for the Junit tests in FileSenderTest
 * 
 * @author Michael
 *
 */
public class FileSenderTestClient extends Thread {

	private final int BUFFER_SIZE = 4096;

	private int port;
	private InetAddress ip;
	private String fileDestination;
	private long fileSize;

	public FileSenderTestClient(InetAddress ip, int port,
			String fileDestination, long fileSize) {
		this.ip = ip;
		this.port = port;
		this.fileDestination = fileDestination;
		this.fileSize = fileSize;
	}

	@Override
	public void run() {
		byte[] buffer = new byte[BUFFER_SIZE];
		int readBytes = 0;
		try (ServerSocket serverSocket = new ServerSocket(port, 0, ip);
				Socket socket = serverSocket.accept();
				BufferedInputStream reader = new BufferedInputStream(
						socket.getInputStream());
				BufferedOutputStream writer = new BufferedOutputStream(
						new FileOutputStream(new File(fileDestination)))) {
			while (fileSize > 0) {
				if (fileSize < buffer.length) {
					readBytes = Utils.readThisLength(reader, buffer, 0,
							(int) fileSize);
				} else {
					readBytes = Utils.readThisLength(reader, buffer, 0,
							buffer.length);
				}
				writer.write(buffer, 0, readBytes);
				fileSize -= readBytes;
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
