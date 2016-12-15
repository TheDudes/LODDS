package studyproject.Test.Lvl.Mid;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;

import studyproject.API.Lvl.Low.Responses;

/**
 * counterpart for the FileConnectionTest junit tests
 * 
 * @author Michael
 *
 */
public class FileConnectionTestServer extends Thread {

	private InetAddress ip;
	private int port;
	private String fileName;
	private long startIndex;
	private long endIndex;

	/**
	 * 
	 * @param ip the ip on which to listen for the client
	 * @param port the port on wich to listen for the client
	 * @param fileName the name of the file to send
	 * @param startIndex the index from which to send the file from
	 * @param endIndex the last index of the part of the file to be sent
	 */
	public FileConnectionTestServer(InetAddress ip, int port, String fileName, long startIndex, long endIndex) {
		this.ip = ip;
		this.port = port;
		this.fileName = fileName;
		this.startIndex = startIndex;
		this.endIndex = endIndex;
	}

	@Override
	public void run() {
		try (ServerSocket servSocket = new ServerSocket(port, 0, ip);
				Socket socket = servSocket.accept();
				BufferedOutputStream socketStream = new BufferedOutputStream(socket.getOutputStream());
				BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
				FileInputStream fileStream = new FileInputStream(new File(fileName))) {
			// read the "command" sent by the client, irrelevant since in this
			// testcase we already know what we
			// are supposed to do. Not doing this causes an inconsistent error
			// where recv fails in the Client in Handles.handleFile
			in.readLine();
			Responses.respondFile(socketStream, fileStream, startIndex, endIndex);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
