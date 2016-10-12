package studyproject.API.Lvl.Mid;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.Socket;
import java.nio.file.Paths;

import studyproject.API.Core.File.FileInfo;
import studyproject.API.Lvl.Low.Responses;
import studyproject.API.Lvl.Mid.Core.UserInfo;

public class FileSenderThread extends Thread {
	private UserInfo userToSendTo;
	private long size;
	private long startIndex = 0;
	private long endIndex = 0;
	private FileInfo fileInfo;

	public FileSenderThread(UserInfo userToSendTo, FileInfo fileInfo, long startIndex, long endIndex) {
		this.userToSendTo = userToSendTo;
		this.fileInfo = fileInfo;
		this.startIndex = startIndex;
		this.endIndex = endIndex;

	}

	public FileSenderThread(UserInfo userToSendTo, FileInfo fileInfo) {
		this.userToSendTo = userToSendTo;
		this.fileInfo = fileInfo;
	}

	@Override
	public void run() {
		int returnValue;
		try (Socket socket = new Socket(userToSendTo.getIpAddress(), userToSendTo.getPort());
				BufferedOutputStream outStream = new BufferedOutputStream(socket.getOutputStream());
				FileInputStream fileInStream = new FileInputStream(
						new File((Paths.get(fileInfo.parentDirectory).resolve(fileInfo.fileName)).toString()));) {

			if (endIndex == 0) {
				returnValue = Responses.respondFile(outStream, fileInStream, startIndex, size);
			} else {
				returnValue = Responses.respondFile(outStream, fileInStream, startIndex, endIndex);
			}
			if (returnValue != 0) {
				// TODO error handling
			}

		} catch (IOException e) {
		}
	}
}
