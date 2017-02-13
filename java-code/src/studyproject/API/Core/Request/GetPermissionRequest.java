package studyproject.API.Core.Request;

import java.net.Socket;

/**
 * class to store the information of a get send permission request in
 * @author Michael
 *
 */
public class GetPermissionRequest implements Request {

	public Socket socket;
	public String checksum;
	public long fileSize;
	public long timeout;
	public String fileName;
	
	@Override
	public RequestType getType() {
		return RequestType.GET_SEND_PERMISSION;
	}

}
