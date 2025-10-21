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
package net.codecadenza.runtime.ddt.model.xml;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import net.codecadenza.runtime.ddt.model.MethodInvocation;
import net.codecadenza.runtime.ddt.model.Parameter;
import net.codecadenza.runtime.ddt.model.ReturnValue;

/**
 * <p>
 * XML mapping for {@link MethodInvocation} objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class XMLMethodInvocation implements MethodInvocation {
	private String className;
	private String methodName;
	private UUID groupId;
	private Long timeoutMillis;
	private List<Parameter> parameters = new ArrayList<>();
	private ReturnValue returnValue;
	private String postProcessingStatement;
	private Integer expectedSize;

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.MethodInvocation#getClassName()
	 */
	@Override
	@XmlAttribute(name = "class_name")
	public String getClassName() {
		return className;
	}

	/**
	 * Set the class name
	 * @param className
	 */
	public void setClassName(String className) {
		this.className = className;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.MethodInvocation#getMethodName()
	 */
	@Override
	@XmlAttribute(name = "method_name")
	public String getMethodName() {
		return methodName;
	}

	/**
	 * Set the method name
	 * @param methodName
	 */
	public void setMethodName(String methodName) {
		this.methodName = methodName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.MethodInvocation#getGroupId()
	 */
	@Override
	@XmlAttribute(name = "group_id")
	public UUID getGroupId() {
		return groupId;
	}

	/**
	 * Set the group ID
	 * @param groupId
	 */
	public void setGroupId(UUID groupId) {
		this.groupId = groupId;
	}

	/**
	 * @return the maximum execution time in milliseconds
	 */
	@XmlAttribute(name = "timeout_millis")
	public Long getTimeoutMillis() {
		return timeoutMillis;
	}

	/**
	 * Set the maximum execution time in milliseconds
	 * @param timeoutMillis
	 */
	public void setTimeoutMillis(Long timeoutMillis) {
		this.timeoutMillis = timeoutMillis;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.MethodInvocation#getTimeout()
	 */
	@Override
	public Duration getTimeout() {
		if (getTimeoutMillis() == null)
			return null;

		return Duration.ofMillis(getTimeoutMillis());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.MethodInvocation#getParameters()
	 */
	@Override
	@XmlElementWrapper(name = "parameters")
	@XmlElement(type = XMLParameter.class, name = "parameter")
	public List<Parameter> getParameters() {
		return parameters;
	}

	/**
	 * Set the parameters
	 * @param parameters
	 */
	public void setParameters(List<Parameter> parameters) {
		this.parameters = parameters;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.MethodInvocation#getReturnValue()
	 */
	@Override
	@XmlElement(type = XMLReturnValue.class, name = "return_value")
	public ReturnValue getReturnValue() {
		return returnValue;
	}

	/**
	 * Set the return value
	 * @param returnValue
	 */
	public void setReturnValue(ReturnValue returnValue) {
		this.returnValue = returnValue;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.MethodInvocation#getPostProcessingStatement()
	 */
	@Override
	@XmlElement(name = "post_processing_statement")
	public String getPostProcessingStatement() {
		return postProcessingStatement;
	}

	/**
	 * Set the statement that should be executed after a method has been invoked
	 * @param postProcessingStatement
	 */
	public void setPostProcessingStatement(String postProcessingStatement) {
		this.postProcessingStatement = postProcessingStatement;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.MethodInvocation#getExpectedSize()
	 */
	@Override
	@XmlAttribute(name = "expected_size")
	public Integer getExpectedSize() {
		return expectedSize;
	}

	/**
	 * Set the expected size
	 * @param expectedSize
	 */
	public void setExpectedSize(Integer expectedSize) {
		this.expectedSize = expectedSize;
	}

}
