package studyproject.API.Core.Request;

/**
 * Class to store the information of a get file request in
 * @author Michael
 *
 */
public class GetFileRequest implements Request {

	public String checksum;
	public long startIndex;
	public long endIndex;
	
	@Override
	public RequestType getType() {
		return RequestType.GET_FILE;
	}

}
