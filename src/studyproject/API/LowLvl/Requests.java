package studyproject.API.LowLvl;

import java.net.Socket;

public interface Requests {

	public int getInfoUp(Socket socket, long timestamp);

	public int getInfoAll(Socket socket);

	public int getFile(Socket socket, String checksum, long startIndex, long endIndex);

	public int getInfoLoad(Socket socket);

	public int getSendPermission(Socket socket, long size, String fileName, long timeout);

}
