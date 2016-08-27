package studyproject.API.Core.Request;

public class GetPermissionRequest implements Request {

	public long fileSize;
	public long timeout;
	public String fileName;
	
	@Override
	public RequestType getType() {
		return RequestType.GET_SEND_PERMISSION;
	}

}
