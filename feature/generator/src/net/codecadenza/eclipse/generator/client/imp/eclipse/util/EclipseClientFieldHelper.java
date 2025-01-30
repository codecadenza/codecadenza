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
package net.codecadenza.eclipse.generator.client.imp.eclipse.util;

import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import net.codecadenza.eclipse.generator.client.common.security.ClientParameterHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Helper class that is responsible for adding a field that holds the ID of the logged on user or the ID of the client the user
 * belongs to. In case of an Eclipse RAP application, it is necessary that a respective boundary method parameter is initialized
 * by this field if the method is called from a non-UI thread. Otherwise, a runtime exception will be thrown because calling
 * SingletonUtil.getSessionInstance() from a non-UI thread is not allowed!
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseClientFieldHelper {
	private final Form form;
	private final AbstractJavaSourceGenerator generator;
	private final Project project;
	private final ClientParameterHelper clientParameterHelper;
	private JavaType fieldType;
	private String fieldName;

	/**
	 * Constructor
	 * @param form
	 * @param generator
	 */
	public EclipseClientFieldHelper(Form form, AbstractJavaSourceGenerator generator) {
		this.form = form;
		this.generator = generator;
		this.project = form.getDomainObject().getNamespace().getProject();
		this.clientParameterHelper = new ClientParameterHelper(this.form.getBoundaryMethod());

		if (this.clientParameterHelper.getClientFilterAttribute() != null) {
			this.fieldType = this.clientParameterHelper.getClientFilterAttribute().getDomainAttribute().getJavaType();
			this.fieldName = this.clientParameterHelper.getClientFilterAttribute().getName();
		}
	}

	/**
	 * If necessary, add a field that holds the ID of the client or the logged on user
	 */
	public void addClientField() {
		if (project.hasRAPClient() && clientParameterHelper.getClientFilterAttribute() != null)
			generator.addPrivateField(fieldType.getName(), fieldName).create();
	}

	/**
	 * If necessary, initialize the field with the ID of the client or the logged on user
	 * @return the generated content. An empty string is returned if this field isn't required!
	 */
	public String initClientField() {
		if (project.hasRAPClient() && clientParameterHelper.getClientFilterAttribute() != null)
			return fieldName + " = " + SECURITY_MANAGER + ".getLogOnDTO()."
					+ clientParameterHelper.getClientFilterAttribute().getGetterName() + ";\n";

		return "";
	}

}
