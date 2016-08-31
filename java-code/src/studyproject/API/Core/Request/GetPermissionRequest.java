package studyproject.API.Core.Request;

/**
 * class to store the information of a get send permission request in
 * @author Michael
 *
 */
public class GetPermissionRequest implements Request {

	public long fileSize;
	public long timeout;
	public String fileName;
	
	@Override
	public RequestType getType() {
		return RequestType.GET_SEND_PERMISSION;
	}

}
