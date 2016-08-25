package studyproject.API.LowLvl;

import java.net.Socket;

public interface Requests {

	public Socket getInfoUp(Socket socket, long timestamp);

	public Socket getInfoAll(Socket socket);

	public Socket getFile(Socket socket, String checksum, long startIndex, long endIndex);

	public Socket getInfoLoad(Socket socket);

	public Socket getSendPermission(Socket socket, long size, String fileName, long timeout);

}
