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
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;

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

	private final DomainObject domainObject;

	/**
	 * Constructor
	 * @param bot
	 * @param domainObject
	 * @param title
	 */
	public SingleRecordFormTestDialog(SWTWorkbenchBot bot, DomainObject domainObject, String title) {
		super(bot, title);

		this.domainObject = domainObject;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		for (final DomainAttribute attribute : domainObject.getAllAttributes()) {
			final var fieldLabel = createFieldLabel(attribute.getName());

			if (attribute.getType().equals(DomainAttribute.TYPE_STRING)) {
				final var txtField = bot.textWithLabel(fieldLabel);
				txtField.typeText(DEFAULT_TEXT);
			}
			else if (attribute.getType().equals(DomainAttribute.TYPE_LONG) && !attribute.isPrimaryKey()) {
				final var txtField = bot.textWithLabel(fieldLabel);
				txtField.typeText(DEFAULT_INTEGER);
			}
			else if (attribute.getType().equals(DomainAttribute.TYPE_DOUBLE)
					|| attribute.getType().equals(DomainAttribute.TYPE_BIG_DECIMAL)) {
				final var txtField = bot.textWithLabel(fieldLabel);
				txtField.typeText(DEFAULT_NUMBER);
			}
			else if (attribute.getType().equals(DomainAttribute.TYPE_BOOLEAN)) {
				final var cboField = bot.comboBoxWithLabel(fieldLabel);
				cboField.setSelection(Boolean.TRUE.toString());
			}
			else if (attribute.getType().equals(DomainAttribute.TYPE_LOCAL_DATE)) {
				final var txtField = bot.textWithLabel(fieldLabel);
				txtField.typeText(DATE_FORMATTER.format(Instant.now()));
			}
			else if (attribute.getType().equals(DomainAttribute.TYPE_UUID)) {
				final var txtField = bot.textWithLabel(fieldLabel);
				txtField.typeText(UUID.randomUUID().toString());
			}
			else if (attribute.getType().equals(DomainAttribute.TYPE_DATE)
					|| attribute.getType().equals(DomainAttribute.TYPE_LOCAL_DATE_TIME)
					|| attribute.getType().equals(DomainAttribute.TYPE_CALENDAR)) {
				final var txtField = bot.textWithLabel(fieldLabel);
				txtField.typeText(DATE_TIME_FORMATTER.format(Instant.now()));
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

}
