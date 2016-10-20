package studyproject.API.Lvl.Mid;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.net.Socket;

import studyproject.API.Core.File.Watcher.FileWatcherController;

public class InfoSenderThread extends Thread {
	
	long timestamp;
	FileWatcherController fileWatcherController;
	Socket socket;
	
	/**
	 * Constructor to create new InfoSenderThread object
	 */
	public InfoSenderThread(Socket socket, FileWatcherController fileWatcherController, long timestamp) {
		this.fileWatcherController = fileWatcherController;
		this.timestamp = timestamp;
	}
	
	/**
	 * Start the InfoSenderThread
	 */
	@Override
	public void run() {
		byte[] infoBytes = fileWatcherController.getInfo(timestamp).getBytes();
		BufferedOutputStream bufferedOutputStream;
		try {
			bufferedOutputStream = new BufferedOutputStream(socket.getOutputStream());
			bufferedOutputStream.write(infoBytes);
		} catch (IOException e) {
			// TODO IOException: Error handling
			e.printStackTrace();
		}
	}
}
