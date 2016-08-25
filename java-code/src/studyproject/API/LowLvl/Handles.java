package studyproject.API.LowLvl;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;

import studyproject.API.Core.FileAction;
import studyproject.API.Core.FileInfo;

public class Handles {

	public static int handleInfoUp(BufferedReader socketStream, ArrayList<FileInfo> fileInfos, long timestamp) {
		FileInfo fileInfo;
		int listSize;
		String[] params;
		try {
			params = socketStream.readLine().split(" ");
			timestamp = Long.valueOf(params[1]);
			listSize = Integer.valueOf(params[2]);
			for (int i = 0; i < listSize; i++) {
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
		int listSize;
		String[] params;
		try {
			params = socketStream.readLine().split(" ");
			timestamp = Long.valueOf(params[1]);
			listSize = Integer.valueOf(params[2]);
			for (int i = 0; i < listSize; i++) {
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

	public static int handleFile(BufferedInputStream socketStream, FileInputStream fileStream, long size) {
		return 0;
	}

	public static int handleInfoLoad(BufferedInputStream socketStream, long byteToSend) {
		return 0;
	}

	public static int handleSendPermission(BufferedInputStream socketStream, long timout, FileInputStream fileStream) {
		return 0;
	}
}
