package studyproject.API.Core.Request;

public class GetFileRequest implements Request {

	public String checksum;
	public long startIndex;
	public long endIndex;
	
	@Override
	public RequestType getType() {
		return RequestType.GET_FILE;
	}

}
