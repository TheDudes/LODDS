package studyproject.API.Lvl.Mid;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.Paths;

import studyproject.API.Lvl.Low.Responses;

public class GetFileWPThread extends Thread {

	private Socket socket;
	private String pathToSaveTo;
	private long fileSize;
	private String fileName;

	public GetFileWPThread(Socket socket, String pathToSaveTo, String fileName, long fileSize) {
		this.socket = socket;
		this.pathToSaveTo = pathToSaveTo;
		this.fileSize = fileSize;
		this.fileName = fileName;
	}

	@Override
	public void run() {
		FileOutputStream fileOutStream = null;
		
		try {
			//Create the parentDirectory and the file, if it does not exist 
			Files.createDirectories(Paths.get(pathToSaveTo));
			Files.createFile(Paths.get(pathToSaveTo + "/" + fileName));
			//create the fileoutputstream to write the file to the filesystem
			fileOutStream = new FileOutputStream((Paths.get(pathToSaveTo).resolve(fileName)).toString());

			if (Responses.respondSendPermission(socket, fileOutStream, fileSize) == 1)
				;
			// TODO Handle IO Error

		} catch (FileNotFoundException e) {
			// TODO Error Handling
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Error Handling
			e.printStackTrace();
		} finally {
			try {
				if (fileOutStream != null)
					fileOutStream.close();
			} catch (IOException e) {
				// TODO Error Handling
			}
		}
	}
}
