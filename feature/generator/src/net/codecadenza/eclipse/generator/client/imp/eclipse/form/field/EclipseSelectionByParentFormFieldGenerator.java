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

import static net.codecadenza.eclipse.shared.Constants.COMBO_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_FIND_BY_ID;

import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;

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
public class EclipseSelectionByParentFormFieldGenerator extends AbstractEclipseFieldGenerator {
	private final DTOBean listDTO;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public EclipseSelectionByParentFormFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
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

		if (listDTO.getPKAttribute().getDomainAttribute().getJavaType().getNamespace() != null)
			formGenerator.importPackage(listDTO.getPKAttribute().getDomainAttribute().getJavaType().getNamespace().toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		final DTOBeanAttribute pkAttr = listDTO.getPKAttribute();

		String attributeName = field.getName().substring(COMBO_PREFIX.length());
		attributeName = attributeName.substring(0, 1).toLowerCase() + attributeName.substring(1);

		formGenerator.addPrivateField(pkAttr.getDomainAttribute().getJavaType().getName(), attributeName).withFinalModifier()
				.create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getSaveDataFragment(java.lang.String)
	 */
	@Override
	public String getSaveDataFragment(String objectName) {
		final var b = new StringBuilder();
		final String setter = field.getDTOAttribute().getModelSetterName();
		final String attributeName = field.getDTOAttribute().getName();
		String selAttrName = field.getName().substring(COMBO_PREFIX.length());
		selAttrName = attributeName.substring(0, 1).toLowerCase() + selAttrName.substring(1);

		if (project.isBoundaryMode())
			b.append(objectName + "." + setter + "(new " + listDTO.getModelClassName() + "(" + selAttrName + "));\n\n");
		else {
			final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(field.getPanel().getForm().getDomainObject());
			final var declarationGenerator = new ServiceDeclarationGenerator(formGenerator, boundaryBean);
			final String serviceName = declarationGenerator.getServiceName();

			b.append(objectName + "." + setter + "(" + serviceName + "." + REPO_METHOD_NAME_FIND_BY_ID);
			b.append("(" + listDTO.getModelClassName() + ".class, " + selAttrName + "));\n\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.AbstractEclipseFieldGenerator#
	 * getFieldDefinitionFragment(boolean)
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
