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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.form.field;

import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.dto.DTOBean;

/**
 * <p>
 * Generator for fields that are initialized with the ID of the logged on user
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFLogOnFieldGenerator extends AbstractJSFFieldGenerator {
	private final DTOBean dto;
	private final DTOBean logOnDTO;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JSFLogOnFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.dto = field.getDTOAttribute().getReferencedDTOBean();

		// We assume that the appropriate DTO exists!
		this.logOnDTO = dto.getNamespace().getProject().getApplicationLogOnDTO();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (dto.getNamespace().getProject().isBoundaryMode())
			formGenerator.importPackage(dto.getNamespace().toString());
		else
			formGenerator.importPackage(dto.getDomainObject().getNamespace().toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getCreateInitializationFragment()
	 */
	@Override
	public String getCreateInitializationFragment() {
		return getDefaultInitializationFragment();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getDefaultInitializationFragment()
	 */
	@Override
	public String getDefaultInitializationFragment() {
		final var b = new StringBuilder();
		final String setter = field.getDTOAttribute().getModelSetterName();

		b.append(modelObjectName + "." + setter + "(new " + dto.getModelClassName() + "(" + USER_SESSION_BEAN);
		b.append(".getPrincipal()." + logOnDTO.getPKAttribute().getGetterName() + "));\n");

		return b.toString();
	}

}
