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

import java.util.Map;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;

/**
 * <p>
 * Generator for element collection editors
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularElementCollectionEditorGenerator extends AbstractAngularFieldGenerator {
	/**
	 * Constructor
	 * @param field
	 * @param formatter
	 */
	public AngularElementCollectionEditorGenerator(FormField field, AngularContentFormatter formatter) {
		super(field, formatter, field.isReadonly());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#getImports()
	 */
	@Override
	public Map<String, String> getImports() {
		// Return an empty map in order to skip the import of the @angular/forms Validators class!
		return Map.of();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#addBindingFragment()
	 */
	@Override
	public void addBindingFragment() {
		final var fieldDef = new StringBuilder();
		fieldDef.append("this.addControl('" + field.getDTOAttribute().getName() + "'");

		if (formType == FormTypeEnumeration.CREATE || formType == FormTypeEnumeration.ADD)
			fieldDef.append(", [], " + disabled + ", []");

		fieldDef.append(");");

		formatter.addLine(fieldDef.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#addFieldToTemplate()
	 */
	@Override
	public void addFieldToTemplate() {
		if (!field.isVisible())
			return;

		final var controlContainer = new StringBuilder();
		controlContainer.append("<cc-form-control-container [formGroup]=\"formGroup\" ");
		controlContainer.append("name=\"" + field.getDTOAttribute().getName() + "\" [span]=\"true\">");

		formatter.addLine(controlContainer.toString());
		formatter.increaseIndent();
		formatter.addLine(addControlToTemplate());
		formatter.decreaseIndent();
		formatter.addLine("</cc-form-control-container>");
		formatter.addBlankLine();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#addControlToTemplate()
	 */
	@Override
	protected String addControlToTemplate() {
		final var control = new StringBuilder();
		control.append("<cc-element-collection-editor ");
		control.append("id=\"" + field.getName() + "\" formControlName=\"" + field.getDTOAttribute().getName() + "\" fieldType=\"");

		if (domainAttr.getJavaType().isString() || domainAttr.getJavaType().isUUID())
			control.append("STRING");
		else if (domainAttr.getJavaType().isTemporalType()) {
			if (domainAttr.getJavaType().isLocalDate() || domainAttr.getTemporalType() == TemporalTypeEnumeration.DATE)
				control.append("DATE");
			else
				control.append("DATE_TIME");
		}
		else if (domainAttr.getJavaType().isIntegerOrLong())
			control.append("INTEGER");
		else if (domainAttr.getJavaType().isChar())
			control.append("CHARACTER");
		else
			control.append("DECIMAL");

		control.append("\"");

		if (domainAttr.getCollectionType() == CollectionTypeEnumeration.SET)
			control.append(" [uniqueElements]=\"true\"");

		if (readonly || disabled)
			control.append(" [readOnly]=\"true\"");

		control.append("></cc-element-collection-editor>");

		return control.toString();
	}

}
