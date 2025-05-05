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
package net.codecadenza.eclipse.testing.dialog.guitest;

import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.UUID;
import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import net.codecadenza.eclipse.testing.domain.AssociationType;
import net.codecadenza.eclipse.testing.domain.DomainAttribute;
import net.codecadenza.eclipse.testing.domain.DomainObject;
import net.codecadenza.eclipse.testing.domain.ElementCollectionType;
import net.codecadenza.eclipse.testing.domain.FormType;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotText;

/**
 * <p>
 * Dialog for creating GUI test actions of a single-record form
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SingleRecordFormTestDialog extends AbstractDialog {
	private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("dd.MM.yyyy")
			.withZone(ZoneId.systemDefault());
	private static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern("dd.MM.yyyy HH-mm-ss")
			.withZone(ZoneId.systemDefault());
	private static final String DEFAULT_TEXT = "text";
	private static final String DEFAULT_INTEGER = "1";
	private static final String DEFAULT_ITEM = "item";
	private static final String DEFAULT_NUMBER = "1.0";
	private static final String LBL_ELEMENT_COLLECTION_EDITOR_CHECK = "Value for an expected element";
	private static final String LBL_ELEMENT_COLLECTION_EDITOR_ADD = "Value for a new element";

	private final DomainObject domainObject;
	private final FormType formType;

	/**
	 * Constructor
	 * @param bot
	 * @param domainObject
	 * @param title
	 * @param formType
	 */
	public SingleRecordFormTestDialog(SWTWorkbenchBot bot, DomainObject domainObject, String title, FormType formType) {
		super(bot, title);

		this.domainObject = domainObject;
		this.formType = formType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		for (final DomainAttribute attribute : domainObject.getAllAttributes()) {
			final var fieldLabel = createFieldLabel(attribute.getName());
			final var typeName = attribute.getTypeName();
			final var defaultValue = getDefaultValue(attribute.getType());

			// It is assumed that fields mapped to auto-generated primary key attributes aren't visible
			if (defaultValue == null
					|| (typeName.equals(DomainAttribute.TYPE_LONG) && attribute.isPrimaryKey() && formType == FormType.CREATE))
				continue;

			if (attribute.getElementCollectionType() != ElementCollectionType.NONE) {
				final SWTBotText txtField;

				if (formType == FormType.READONLY)
					txtField = bot.textWithLabel(LBL_ELEMENT_COLLECTION_EDITOR_CHECK);
				else
					txtField = bot.textWithLabel(LBL_ELEMENT_COLLECTION_EDITOR_ADD);

				txtField.typeText(defaultValue);
				bot.button(CMD_ADD).click();
			}
			else if (typeName.equals(DomainAttribute.TYPE_BOOLEAN)) {
				final var cboField = bot.comboBoxWithLabel(fieldLabel);
				cboField.setSelection(defaultValue);
			}
			else {
				final var txtField = bot.textWithLabel(fieldLabel);
				txtField.typeText(defaultValue);
			}
		}

		for (final var enumAssociation : domainObject.getEnumAssociations()) {
			final var fieldLabel = createFieldLabel(enumAssociation.getName());
			final var literal = enumAssociation.getTarget().getLiterals().get(0);

			final var cboField = bot.comboBoxWithLabel(fieldLabel);
			cboField.setSelection(literal);
		}

		for (final var domainAssociation : domainObject.getAssociations()) {
			if (domainAssociation.getType() != AssociationType.MANY_TO_ONE)
				continue;

			final var fieldLabel = createFieldLabel(domainAssociation.getTarget().getLabel());

			final var txtField = bot.textWithLabel(fieldLabel);
			txtField.typeText(DEFAULT_ITEM);
		}

		bot.button(CMD_OK).click();
	}

	/**
	 * Create a field label based on the provided name
	 * @param name
	 * @return the label of a field
	 */
	private String createFieldLabel(String name) {
		return name.substring(0, 1).toUpperCase() + name.substring(1) + " :";
	}

	/**
	 * Get the default value that should be entered into a field
	 * @param typeName
	 * @return the default value or null if the field should not be set (e.g. in the case of an enumeration)
	 */
	private String getDefaultValue(String typeName) {
		if (typeName.equals(DomainAttribute.TYPE_STRING))
			return DEFAULT_TEXT;
		else if (typeName.equals(DomainAttribute.TYPE_LONG))
			return DEFAULT_INTEGER;
		else if (typeName.equals(DomainAttribute.TYPE_DOUBLE) || typeName.equals(DomainAttribute.TYPE_BIG_DECIMAL))
			return DEFAULT_NUMBER;
		else if (typeName.equals(DomainAttribute.TYPE_LOCAL_DATE))
			return DATE_FORMATTER.format(Instant.now());
		else if (typeName.equals(DomainAttribute.TYPE_DATE) || typeName.equals(DomainAttribute.TYPE_LOCAL_DATE_TIME)
				|| typeName.equals(DomainAttribute.TYPE_CALENDAR))
			return DATE_TIME_FORMATTER.format(Instant.now());
		else if (typeName.equals(DomainAttribute.TYPE_UUID))
			return UUID.randomUUID().toString();
		else if (typeName.equals(DomainAttribute.TYPE_BOOLEAN))
			return Boolean.TRUE.toString();

		return null;
	}

}
