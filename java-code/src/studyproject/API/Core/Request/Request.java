package studyproject.API.Core.Request;

/**
 * Interface as parent for all Requests
 * The RequestType reveals which class is used
 * @author Michael
 *
 */
public interface Request {

	public RequestType getType();
	
}
