package studyproject.API.Lvl.Mid;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.net.Socket;

import studyproject.API.Core.File.Watcher.FileWatcherController;

/**
 * This class provides functionality for the InfoSenderThread. The thread shares
 * information about his files to other clients
 * 
 * @author chris
 *
 */
public class InfoSenderThread extends Thread {

	long timestamp;
	FileWatcherController fileWatcherController;
	Socket socket;

	/**
	 * Constructor to create new InfoSenderThread object
	 * 
	 * @param socket
	 *            A connected socket to the information requesting partner
	 * @param fileWatcherController
	 *            FileWatcherController-Object that is watching the shared files
	 *            directory
	 * @param timestamp
	 *            is a timestamp in milliseconds as long on which the content of
	 *            the shared information is based on
	 */
	public InfoSenderThread(Socket socket, FileWatcherController fileWatcherController, long timestamp) {
		this.fileWatcherController = fileWatcherController;
		this.timestamp = timestamp;
		this.socket = socket;
	}

	/**
	 * Start the InfoSenderThread
	 */
	@Override
	public void run() {
		byte[] infoBytes = fileWatcherController.getInfo(timestamp).getBytes();
		try (BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(socket.getOutputStream())) {
			bufferedOutputStream.write(infoBytes);
			bufferedOutputStream.flush();
			socket.close();
		} catch (IOException e) {
			// TODO IOException: Error handling
			e.printStackTrace();
		}
	}
}
