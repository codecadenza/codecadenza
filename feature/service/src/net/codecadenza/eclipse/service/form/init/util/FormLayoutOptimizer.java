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
package net.codecadenza.eclipse.service.form.init.util;

import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormPanel;
import org.eclipse.emf.common.util.ECollections;

/**
 * <p>
 * Utility class to create an optimal initial layout
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FormLayoutOptimizer {
	/**
	 * Prevent instantiation
	 */
	private FormLayoutOptimizer() {

	}

	/**
	 * Optimize the layout of the given form panel
	 * @param p
	 */
	public static void optimizeLayout(FormPanel p) {
		int totalFieldCount = 0;
		int invisibleFieldCount = 0;
		int multiLineFieldCount = 0;
		int simpleFieldCount = 0;

		// Calculate the number of visible, invisible, mandatory and non-mandatory form fields
		for (final FormField f : p.getFields()) {
			totalFieldCount++;

			if (f.isHidden())
				invisibleFieldCount++;
			else if (f.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_LABEL
					|| f.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_TEXT) {
				// A multi-line field can be mandatory, but it should be pushed to bottom anyway!
				multiLineFieldCount++;
			}
			else
				simpleFieldCount++;
		}

		// Move invisible fields to the bottom
		for (final FormField f : p.getFields()) {
			if (f.isHidden()) {
				while (f.getRowIndex() != totalFieldCount)
					moveDownField(f);
			}
		}

		// Move multi-line fields down
		for (final FormField f : p.getFields()) {
			if (f.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_LABEL
					|| f.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_TEXT) {
				final int lastFieldIndex = totalFieldCount - invisibleFieldCount;

				while (f.getRowIndex() != lastFieldIndex)
					moveDownField(f);
			}
		}

		// Now all non-mandatory fields should be placed above the invisible fields and multi-line text fields should be below of the
		// mandatory fields!
		for (final FormField f : p.getFields()) {
			if (f.isHidden() || f.isMandatory())
				continue;

			if (f.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_LABEL
					|| f.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_TEXT)
				continue;

			final int lastFieldIndex = totalFieldCount - (invisibleFieldCount + multiLineFieldCount);

			while (f.getRowIndex() != lastFieldIndex)
				moveDownField(f);
		}

		FormField pkField = null;

		// Move the field that represents the primary key to the top
		for (final FormField f : p.getFields()) {
			if (f.isHidden())
				continue;

			if (!(f.getDTOAttribute().equals(p.getForm().getDTO().getPKAttribute())))
				continue;

			pkField = f;

			while (pkField.getRowIndex() != 1)
				moveUpField(pkField);
		}

		// Move the field that represents the display attribute to the top
		if (p.getForm().getDTO().getDisplayAttribute() != null) {
			for (final FormField f : p.getFields()) {
				if (f.isHidden())
					continue;

				if (!(f.getDTOAttribute().equals(p.getForm().getDTO().getDisplayAttribute())))
					continue;

				int firstFieldIndex = 1;

				// If a primary key field exists we will place this field into the second row!
				if (pkField != null)
					firstFieldIndex = 2;

				while (f.getRowIndex() != firstFieldIndex)
					moveUpField(f);
			}
		}

		ECollections.sort(p.getFields(), (a, b) -> {
			if (a.getRowIndex() == b.getRowIndex())
				return 0;

			if (a.getRowIndex() > b.getRowIndex())
				return 1;

			return -1;
		});

		// Now we decide if the form should be initialized with two columns. The decision is more or less arbitrarily!
		if (simpleFieldCount > 4 || (simpleFieldCount > 1 && multiLineFieldCount > 0)) {
			boolean lastFieldSpan = false;
			int lastRowIndex = 1;
			boolean secondColumn = false;

			for (final FormField f : p.getFields()) {
				if (lastRowIndex != f.getRowIndex() && !lastFieldSpan && !f.isSpanCols() && !secondColumn) {
					moveUpField(f);

					f.setColIndex(2);
					secondColumn = true;

					for (final FormField field : p.getFields())
						if (field.getRowIndex() > f.getRowIndex())
							field.setRowIndex(field.getRowIndex() - 1);
				}
				else
					secondColumn = false;

				lastRowIndex = f.getRowIndex();
				lastFieldSpan = f.isSpanCols();
			}
		}
	}

	/**
	 * Move the field down
	 * @param field
	 */
	private static void moveDownField(FormField field) {
		// Test if the field can be moved down
		if (field.getRowIndex() == field.getPanel().getFields().size())
			return;

		// Change the row index of the form field that comes next
		for (final FormField f : field.getPanel().getFields()) {
			if (f.getRowIndex() == (field.getRowIndex() + 1)) {
				f.setRowIndex(f.getRowIndex() - 1);
				break;
			}
		}

		field.setRowIndex(field.getRowIndex() + 1);
	}

	/**
	 * Move the field up
	 * @param field
	 */
	private static void moveUpField(FormField field) {
		// Test if the table column field can be moved up
		if (field.getRowIndex() == 1)
			return;

		final int movedRowIndex = field.getRowIndex();

		// Change the column index of the item above
		for (final FormField f : field.getPanel().getFields()) {
			if (f.getRowIndex() == (movedRowIndex - 1)) {
				f.setRowIndex(f.getRowIndex() + 1);
				break;
			}
		}

		field.setRowIndex(field.getRowIndex() - 1);
	}

}
