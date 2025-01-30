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
package net.codecadenza.eclipse.generator.client.common.field;

import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;

/**
 * <p>
 * Abstract base class for all rich-client form field generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractRichClientFieldGenerator extends AbstractClientFieldGenerator {
	protected RichClientI18NGenerator i18n;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	protected AbstractRichClientFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);
	}

	/**
	 * @return the I18N generator
	 */
	public RichClientI18NGenerator getI18n() {
		return i18n;
	}

	/**
	 * @param i18n
	 */
	public void setI18n(RichClientI18NGenerator i18n) {
		this.i18n = i18n;
	}

	/**
	 * Create the code fragment that displays field validation errors
	 * @param message
	 * @param hasTitleArea
	 * @return the generated content
	 */
	protected abstract String getFieldValidationMessageFragment(String message, boolean hasTitleArea);

}
