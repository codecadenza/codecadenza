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
package net.codecadenza.eclipse.generator.client.imp.eclipse.form.field;

import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_FIND_BY_ID;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.security.EclipseSecurityHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;

/**
 * <p>
 * Generator for fields that are initialized with the ID of the client the currently logged on user belongs to
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseSelectionByClientFieldGenerator extends AbstractEclipseFieldGenerator {
	private final DTOBean logOnDTO;
	private final BoundaryBean logOnBean;
	private boolean logOnExists;
	private final EclipseSecurityHelper securityHelper;
	private final DTOBean listDTO;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public EclipseSelectionByClientFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
		this.securityHelper = new EclipseSecurityHelper(project);
		this.logOnDTO = project.getApplicationLogOnDTO();
		this.logOnBean = project.getLogOnBoundary();

		if (logOnDTO != null && logOnBean != null)
			this.logOnExists = true;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!logOnExists)
			return;

		formGenerator.addImports(securityHelper.getSecurityManagerImports());

		if (project.isBoundaryMode())
			formGenerator.importPackage(listDTO.getNamespace().toString());
		else
			formGenerator.importPackage(listDTO.getDomainObject().getNamespace().toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getSaveDataFragment(java.lang.String)
	 */
	@Override
	public String getSaveDataFragment(String objectName) {
		final var b = new StringBuilder();
		final String setter = field.getDTOAttribute().getModelSetterName();

		if (!logOnExists)
			return "";

		final DTOBeanAttribute clientPKAttr = logOnDTO.getClientPKAttribute();

		if (project.isBoundaryMode()) {
			b.append(objectName + "." + setter + "(new " + listDTO.getName() + "(");
		}
		else {
			final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(field.getPanel().getForm().getDomainObject());
			final var declarationGenerator = new ServiceDeclarationGenerator(formGenerator, boundaryBean);
			final String serviceName = declarationGenerator.getServiceName();

			b.append(objectName + "." + setter + "(" + serviceName + "." + REPO_METHOD_NAME_FIND_BY_ID);
			b.append("(" + listDTO.getModelClassName() + ".class, ");
		}

		b.append(SECURITY_MANAGER + ".getLogOnDTO()." + clientPKAttr.getGetterName() + "));\n\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.AbstractEclipseFieldGenerator#getFieldDefinitionFragment(
	 * boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.AbstractEclipseFieldGenerator#
	 * getFieldDefinitionFragment()
	 */
	@Override
	public String getFieldDefinitionFragment() {
		return null;
	}

}
