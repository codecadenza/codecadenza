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

/**
 * <p>
 * Generator for checkbox fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularCheckBoxFieldGenerator extends AbstractAngularFieldGenerator {
	/**
	 * Constructor
	 * @param field
	 * @param formatter
	 */
	public AngularCheckBoxFieldGenerator(FormField field, AngularContentFormatter formatter) {
		super(field, formatter);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#addControlToTemplate()
	 */
	@Override
	protected String addControlToTemplate() {
		final var control = new StringBuilder();
		control.append("<input type=\"checkbox\" formControlName=\"" + field.getDTOAttribute().getName() + "\" ");
		control.append("id=\"" + field.getName() + "\"");

		if (readonly || disabled)
			control.append(" [attr.disabled]=\"true\"");

		control.append("/>");

		return control.toString();
	}

}
