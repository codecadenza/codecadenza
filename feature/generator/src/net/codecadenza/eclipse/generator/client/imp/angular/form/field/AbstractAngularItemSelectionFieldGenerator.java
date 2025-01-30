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
package net.codecadenza.eclipse.generator.client.imp.angular.form.field;

import java.util.Collections;
import java.util.List;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.common.TypeScriptFieldGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.service.AngularServiceInvocationGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;

/**
 * <p>
 * Base class for all form field generators that are capable to handle the selection of items
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractAngularItemSelectionFieldGenerator extends AbstractAngularFieldGenerator {
	protected final boolean filterItems;
	protected final DTOBean listDTO;
	protected final DTOBeanAttribute displayAttr;
	protected final BoundaryBean boundaryBean;
	protected final BoundaryMethod method;
	protected final String itemListName;
	protected final String fetchItemsMethodName;

	/**
	 * Constructor
	 * @param field
	 * @param formatter
	 * @param filterItems
	 */
	protected AbstractAngularItemSelectionFieldGenerator(FormField field, AngularContentFormatter formatter, boolean filterItems) {
		super(field, formatter);

		this.filterItems = filterItems;
		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
		this.displayAttr = listDTO.getDisplayAttribute() == null ? listDTO.getPKAttribute() : listDTO.getDisplayAttribute();
		this.itemListName = field.getDTOAttribute().getName() + "List";
		this.boundaryBean = project.getBoundaryByDomainObject(listDTO.getDomainObject());
		this.method = boundaryBean.getBoundaryMethodByReturnType(listDTO, BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER);

		if (filterItems)
			this.fetchItemsMethodName = "search" + field.getDTOAttribute().getUpperCaseName() + "List";
		else
			this.fetchItemsMethodName = "get" + field.getDTOAttribute().getUpperCaseName() + "List";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#getListDTO()
	 */
	@Override
	public DTOBean getListDTO() {
		if (!field.isVisible())
			return null;

		return listDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#getFields()
	 */
	@Override
	public List<TypeScriptFieldGenerator> getFields() {
		if (!field.isVisible())
			return Collections.emptyList();

		return Collections.singletonList(
				new TypeScriptFieldGenerator(itemListName + ": " + listDTO.getName() + "[]", null, formatter).withDefaultValue("[]"));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#
	 * getFieldInitializationFragment()
	 */
	@Override
	public String getFieldInitializationFragment() {
		if (!field.isVisible() || filterItems)
			return null;

		return "this." + fetchItemsMethodName + "();";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#isAuthServiceRequired()
	 */
	@Override
	public boolean isAuthServiceRequired() {
		if (!field.isVisible())
			return false;

		return new AngularServiceInvocationGenerator(method, listDTO).isAuthServiceRequired();
	}

	/**
	 * @return the filter parameter for the back-end invocation
	 */
	protected String getInvocationParameter() {
		if (filterItems)
			return "event.query";

		return "'%'";
	}

}
