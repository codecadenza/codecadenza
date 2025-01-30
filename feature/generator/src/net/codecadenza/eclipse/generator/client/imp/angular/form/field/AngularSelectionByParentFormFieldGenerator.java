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

import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.dto.DTOBean;

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
public class AngularSelectionByParentFormFieldGenerator extends AbstractAngularFieldGenerator {
	protected final DTOBean listDTO;

	/**
	 * Constructor
	 * @param field
	 * @param formatter
	 */
	public AngularSelectionByParentFormFieldGenerator(FormField field, AngularContentFormatter formatter) {
		super(field, formatter);

		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#addBindingFragment()
	 */
	@Override
	public void addBindingFragment() {
		if (disabled)
			return;

		final var fieldDef = new StringBuilder();
		fieldDef.append("this.addControl('" + field.getDTOAttribute().getName() + "'");
		fieldDef.append(", [], false, { " + listDTO.getPKAttribute().getName() + ": this.parentObjectId });");

		formatter.addLine(fieldDef.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#addFieldToTemplate()
	 */
	@Override
	public void addFieldToTemplate() {
		// No implementation required!
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#addControlToTemplate()
	 */
	@Override
	protected String addControlToTemplate() {
		return null;
	}

}
