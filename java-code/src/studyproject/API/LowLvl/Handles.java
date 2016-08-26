package studyproject.API.LowLvl;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import studyproject.API.Core.FileAction;
import studyproject.API.Core.FileInfo;
import studyproject.API.Core.Timestamp;
import studyproject.API.Core.Utils;

public class Handles {
	
	private static final int BUFFERSIZE = 4096;

	public static int handleInfoUp(BufferedReader socketStream, ArrayList<FileInfo> fileInfos, Timestamp timestamp) {
		FileInfo fileInfo;
		String[] params;
		try {
			params = socketStream.readLine().split(" ");
			timestamp.value = Long.valueOf(params[1]);
			for (int i = 0; i < Integer.valueOf(params[2]); i++) {
				params = socketStream.readLine().split(" ");
				fileInfo = new FileInfo();
				if (params[0].equals(FileAction.add.toString())) {
					fileInfo.fileAction = FileAction.add;
				} else {
					fileInfo.fileAction = FileAction.del;
				}
				fileInfo.checksum = params[1];
				fileInfo.size = Long.valueOf(params[2]);
				fileInfo.fileName = params[3];
				fileInfos.add(fileInfo);
			}
		} catch (IOException e) {
			return -1;
		} catch (NumberFormatException e) {
			return -2;
		}
		return 0;
	}

	public static int handleInfoAll(BufferedReader socketStream, ArrayList<FileInfo> fileInfos, long timestamp) {
		FileInfo fileInfo;
		String[] params;
		try {
			params = socketStream.readLine().split(" ");
			timestamp = Long.valueOf(params[1]);
			for (int i = 0; i < Integer.valueOf(params[2]); i++) {
				params = socketStream.readLine().split(" ");
				fileInfo = new FileInfo();
				if (params[0].equals(FileAction.add.toString())) {
					fileInfo.fileAction = FileAction.add;
				} else {
					fileInfo.fileAction = FileAction.del;
				}
				fileInfo.checksum = params[1];
				fileInfo.size = Long.valueOf(params[2]);
				fileInfo.fileName = params[3];
				fileInfos.add(fileInfo);
			}
		} catch (IOException e) {
			return -1;
		} catch (NumberFormatException e) {
			return -2;
		}
		return 0;
	}

	public static int handleFile(BufferedInputStream socketStream, FileOutputStream fileStream, long size) {
		try {
			byte[] byteArray = new byte[BUFFERSIZE];
			while(size > 0) {
				int readSize = Utils.readThisLength(socketStream, byteArray, 0, byteArray.length);
				fileStream.write(byteArray);
				size -= readSize;
			}
		} catch (IOException e) {
			return -2;
		}
		return 0;
	}

	public static long handleInfoLoad(BufferedReader socketStream) {
		long byteToSend;
		try {
			byteToSend =  Long.valueOf(socketStream.readLine());
		} catch (NumberFormatException e) {
			return -1;
		} catch (IOException e) {
			return -2;
		}
		return byteToSend;
	}

	public static int handleSendPermission(BufferedReader socketStream) { //, long timout, FileInputStream fileStream) {
		try {
			String s = socketStream.readLine();
			if (!s.equals("OK")) {
				return -1;
			}
		} catch (IOException e) {
			return -2;
		}
		return 0;
	}
}
