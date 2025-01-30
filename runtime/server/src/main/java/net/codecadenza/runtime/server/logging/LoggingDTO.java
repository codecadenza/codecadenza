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
package net.codecadenza.runtime.server.logging;

/**
 * <p>
 * Internal data transfer object for logging
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class LoggingDTO {
	private String className;
	private String methodName;
	private String message;
	private Long duration;
	private Throwable throwable;

	/**
	 * Constructor
	 */
	public LoggingDTO() {

	}

	/**
	 * Constructor to be used within interceptor
	 * @param className
	 * @param methodName
	 * @param message
	 * @param duration
	 * @param throwable
	 */
	public LoggingDTO(String className, String methodName, String message, Long duration, Throwable throwable) {
		this.className = className;
		this.methodName = methodName;
		this.message = message;
		this.duration = duration;
		this.throwable = throwable;
	}

	/**
	 * Constructor to be used when log is created manually
	 * @param message
	 * @param duration
	 * @param throwable
	 */
	public LoggingDTO(String message, Long duration, Throwable throwable) {
		this.className = new Throwable().fillInStackTrace().getStackTrace()[1].getClassName();
		this.methodName = new Throwable().fillInStackTrace().getStackTrace()[1].getMethodName();
		this.message = message;
		this.duration = duration;
		this.throwable = throwable;
	}

	/**
	 * Constructor to be used when log is created manually
	 * @param message
	 * @param duration
	 */
	public LoggingDTO(String message, Long duration) {
		this.className = new Throwable().fillInStackTrace().getStackTrace()[1].getClassName();
		this.methodName = new Throwable().fillInStackTrace().getStackTrace()[1].getMethodName();
		this.message = message;
		this.duration = duration;
	}

	/**
	 * @return the fully qualified class name
	 */
	public String getClassName() {
		return className;
	}

	/**
	 * @param className
	 */
	public void setClassName(String className) {
		this.className = className;
	}

	/**
	 * @return the method name
	 */
	public String getMethodName() {
		return methodName;
	}

	/**
	 * @param methodName
	 */
	public void setMethodName(String methodName) {
		this.methodName = methodName;
	}

	/**
	 * @return the message
	 */
	public String getMessage() {
		return message;
	}

	/**
	 * @param message
	 */
	public void setMessage(String message) {
		this.message = message;
	}

	/**
	 * @return the duration
	 */
	public Long getDuration() {
		return duration;
	}

	/**
	 * @param duration
	 */
	public void setDuration(Long duration) {
		this.duration = duration;
	}

	/**
	 * @return the exception
	 */
	public Throwable getThrowable() {
		return throwable;
	}

	/**
	 * @param throwable
	 */
	public void setThrowable(Throwable throwable) {
		this.throwable = throwable;
	}

}
