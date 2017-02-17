package studyproject.API.Lvl.Mid;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.logging.Level;

import studyproject.API.Errors.ErrLog;
import studyproject.API.Lvl.Low.Responses;
import studyproject.logging.APILvl;
import studyproject.logging.LogKey;

/**
 * This Thread is used to receive a file if another user sent a "get-send
 * permission" request. Use Thread.start() to send the "OK" flag and start the
 * file transfer.
 * 
 * @author ninti
 *
 */
public class GetFileWPThread extends Thread {

	private Socket socket;
	private String pathToSaveTo;
	private long fileSize;
	private String fileName;

	/**
	 * The constructor to create a new get file with permission thread
	 * 
	 * @param socket
	 *            the socket where the request was sent to
	 * @param pathToSaveTo
	 *            the path to save the file to
	 * @param fileName
	 *            the name of the file which will be received
	 * @param fileSize
	 *            the size of the file which will be received
	 */
	public GetFileWPThread(Socket socket, String pathToSaveTo, String fileName, long fileSize) {
		this.socket = socket;
		this.pathToSaveTo = pathToSaveTo;
		this.fileSize = fileSize;
		this.fileName = fileName;
	}

	@Override
	public void run() {
		FileOutputStream fileOutStream = null;
		int error;
		try {
			// Create the parentDirectory and the file, if it does not exist
			if (!Files.exists(Paths.get(pathToSaveTo + "/" + fileName))) {
				Files.createDirectories(Paths.get(pathToSaveTo));
				Files.createFile(Paths.get(pathToSaveTo + "/" + fileName));
			}
			// create the fileoutputstream to write the file to the filesystem
			fileOutStream = new FileOutputStream((Paths.get(pathToSaveTo).resolve(fileName)).toString());

			if ((error = Responses.respondSendPermission(socket, fileOutStream, fileSize)) != 0)
				ErrLog.log(Level.SEVERE, LogKey.error, APILvl.low, error, getClass().getName() + ".run()");

		} catch (FileNotFoundException e) {
			ErrLog.log(Level.SEVERE, LogKey.error, APILvl.low, getClass().getName() + ".run()",
					"FileNotFoundException thrown " + e.getStackTrace());
		} catch (IOException e) {
			ErrLog.log(Level.SEVERE, LogKey.error, APILvl.low, getClass().getName() + ".run()",
					"IOException thrown " + e.getStackTrace());

		} finally {
			try {
				if (fileOutStream != null)
					fileOutStream.close();
			} catch (IOException e) {
				ErrLog.log(Level.SEVERE, LogKey.error, APILvl.low, getClass().getName() + ".run()",
						"IOException thrown " + e.getStackTrace());
			}
		}
	}
}
