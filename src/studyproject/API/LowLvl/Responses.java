package studyproject.API.LowLvl;

import java.io.FileInputStream;
import java.net.Socket;
import java.util.ArrayList;

import studyproject.API.FileInfo;

public interface Responses {

	public int respondInfoUp(Socket socket, ArrayList<FileInfo> fileInfos);

	public int respondInfoAll(Socket socket, ArrayList<FileInfo> fileInfos);

	public int respondFile(Socket socket, FileInputStream fileStream, long startIndex, long endIndex);

	public int respondInfoLoad(Socket socket, long byteToSend);

	public int respondSendPermission(Socket socket, FileInputStream fileStream, long size);
}
