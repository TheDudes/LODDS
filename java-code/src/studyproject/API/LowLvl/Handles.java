package studyproject.API.LowLvl;

import java.io.FileInputStream;
import java.net.Socket;
import java.util.ArrayList;

import studyproject.API.Core.FileInfo;

public interface Handles {

	public int handleInfoUp(Socket socket, ArrayList<FileInfo> fileInfos);

	public int handleInfoAll(Socket socket, ArrayList<FileInfo> fileInfos);

	public int handleFile(Socket socket, FileInputStream fileStream, long size);

	public int handleInfoLoad(Socket socket, long byteToSend);

	public int handleSendPermission(Socket socket, long timout, FileInputStream fileStream);
}
