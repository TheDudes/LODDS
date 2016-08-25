package studyproject.API.LowLvl;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.util.ArrayList;

import studyproject.API.Core.FileInfo;

public class Handles {

	public static int handleInfoUp(BufferedInputStream socketStream, ArrayList<FileInfo> fileInfos) {
		return 0;
	}

	public static int handleInfoAll(BufferedInputStream socketStream, ArrayList<FileInfo> fileInfos) {
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
