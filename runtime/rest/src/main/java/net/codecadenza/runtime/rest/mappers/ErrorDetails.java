/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.runtime.rest.mappers;

import jakarta.xml.bind.annotation.XmlRootElement;
import java.time.Instant;
import java.util.Objects;

/**
 * <p>
 * Class that contains all information that is sent back to the caller when a REST request has failed
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@XmlRootElement
public class ErrorDetails {
	private int status;
	private String message;
	private String resourcePath;
	private String requestMethod;
	private long timestamp = Instant.now().toEpochMilli();

	/**
	 * @return the HTTP status code
	 */
	public int getStatus() {
		return status;
	}

	/**
	 * Set the HTTP status code
	 * @param status
	 */
	public void setStatus(int status) {
		this.status = status;
	}

	/**
	 * @return the error message
	 */
	public String getMessage() {
		return message;
	}

	/**
	 * Set the error message
	 * @param message
	 */
	public void setMessage(String message) {
		this.message = message;
	}

	/**
	 * @return the resource path
	 */
	public String getResourcePath() {
		return resourcePath;
	}

	/**
	 * Set the resource path
	 * @param resourcePath
	 */
	public void setResourcePath(String resourcePath) {
		this.resourcePath = resourcePath;
	}

	/**
	 * @return the HTTP method (e.g. PUT or GET)
	 */
	public String getRequestMethod() {
		return requestMethod;
	}

	/**
	 * Set the HTTP method
	 * @param requestMethod
	 */
	public void setRequestMethod(String requestMethod) {
		this.requestMethod = requestMethod;
	}

	/**
	 * @return the timestamp
	 */
	public long getTimestamp() {
		return timestamp;
	}

	/**
	 * Set the timestamp
	 * @param timestamp
	 */
	public void setTimestamp(long timestamp) {
		this.timestamp = timestamp;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return Objects.hash(message, requestMethod, resourcePath, status, timestamp);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;

		if (obj == null || getClass() != obj.getClass())
			return false;

		final var other = (ErrorDetails) obj;

		return Objects.equals(message, other.message) && Objects.equals(requestMethod, other.requestMethod)
				&& Objects.equals(resourcePath, other.resourcePath) && status == other.status && timestamp == other.timestamp;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "ErrorDetails [status=" + status + ", message=" + message + ", resourcePath=" + resourcePath + ", requestMethod="
				+ requestMethod + ", timestamp=" + timestamp + "]";
	}

}
