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
package net.codecadenza.eclipse.generator.client.imp.vaadin.form.field;

import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_FIND_BY_ID;

import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;

/**
 * <p>
 * Generator for fields that are initialized by the object ID of a parent form
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinSelectionByParentFormFieldGenerator extends AbstractVaadinFieldGenerator {
	private final DTOBean listDTO;
	private final DTOBeanAttribute pkAttr;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public VaadinSelectionByParentFormFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
		this.pkAttr = listDTO.getPKAttribute();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getFieldTypeName()
	 */
	@Override
	protected String getFieldTypeName() {
		return pkAttr.getDomainAttribute().getJavaType().getName();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (project.isBoundaryMode())
			formGenerator.importPackage(listDTO.getNamespace().toString());
		else
			formGenerator.importPackage(listDTO.getDomainObject().getNamespace().toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getCreateInitializationFragment()
	 */
	@Override
	public String getCreateInitializationFragment() {
		final var b = new StringBuilder();
		final JavaType pkType = pkAttr.getDomainAttribute().getJavaType();
		final String setter = field.getDTOAttribute().getModelSetterName();

		if (project.isBoundaryMode())
			b.append(modelObjectName + "." + setter + "(new " + listDTO.getModelClassName() + "(");
		else {
			final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(field.getPanel().getForm().getDomainObject());
			final var declarationGenerator = new ServiceDeclarationGenerator(formGenerator, boundaryBean);
			final String serviceName = declarationGenerator.getServiceName();

			b.append(modelObjectName + "." + setter + "(" + serviceName + "." + REPO_METHOD_NAME_FIND_BY_ID);
			b.append("(" + listDTO.getModelClassName() + ".class, ");
		}

		b.append("navigator.");

		if (pkType.isLong())
			b.append("getLongIdParameter");
		else if (pkType.isInteger())
			b.append("getIntIdParameter");
		else if (pkType.isUUID())
			b.append("getUuidIdParameter");
		else
			b.append("getStringIdParameter");

		b.append("(event)));\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		// It is not necessary to add a declaration for this field!
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getFieldDefinitionFragment(
	 * boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getBinding(java.lang.String)
	 */
	@Override
	public String getBinding(String binderName) {
		return "";
	}

}
