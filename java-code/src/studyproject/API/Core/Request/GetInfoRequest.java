package studyproject.API.Core.Request;

import studyproject.API.Core.File.InfoList.InfoType;

/**
 * Class to store the information of a get info request in
 * @author Michael
 *
 */
public class GetInfoRequest implements Request {

	public InfoType infoType;
	public long timestamp;
	
	@Override
	public RequestType getType() {
		return RequestType.GET_INFO;
	}

}
