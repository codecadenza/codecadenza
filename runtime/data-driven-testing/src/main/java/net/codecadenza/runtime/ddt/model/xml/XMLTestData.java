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
import jakarta.xml.bind.annotation.XmlRootElement;
import java.util.ArrayList;
import java.util.List;
import net.codecadenza.runtime.ddt.model.MethodInvocation;
import net.codecadenza.runtime.ddt.model.TestData;

/**
 * <p>
 * XML mapping for {@link TestData} objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@XmlRootElement(name = "test_data")
public class XMLTestData implements TestData {
	private String userName;
	private String password;
	private List<MethodInvocation> methodInvocations = new ArrayList<>();

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.TestData#getUserName()
	 */
	@XmlAttribute(name = "user_name")
	@Override
	public String getUserName() {
		return userName;
	}

	/**
	 * Set the user name
	 * @param userName
	 */
	public void setUserName(String userName) {
		this.userName = userName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.TestData#getPassword()
	 */
	@XmlAttribute(name = "password")
	@Override
	public String getPassword() {
		return password;
	}

	/**
	 * Set the password
	 * @param password
	 */
	public void setPassword(String password) {
		this.password = password;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.ddt.model.TestData#getMethodInvocations()
	 */
	@Override
	@XmlElement(type = XMLMethodInvocation.class, name = "method_invocation")
	public List<MethodInvocation> getMethodInvocations() {
		return methodInvocations;
	}

	/**
	 * Set the method invocations
	 * @param methodInvocations
	 */
	public void setMethodInvocations(List<MethodInvocation> methodInvocations) {
		this.methodInvocations = methodInvocations;
	}

}
