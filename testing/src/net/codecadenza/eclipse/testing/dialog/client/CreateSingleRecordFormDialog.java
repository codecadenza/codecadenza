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
package net.codecadenza.eclipse.testing.dialog.client;

import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.allOf;
import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.widgetOfType;
import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.withMnemonic;

import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import net.codecadenza.eclipse.testing.domain.DomainObject;
import net.codecadenza.eclipse.testing.domain.FormType;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotButton;
import org.hamcrest.Matcher;

/**
 * <p>
 * Dialog for creating update forms
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateSingleRecordFormDialog extends AbstractDialog {
	private static final String LBL_FORM_TYPE = "Form type:";
	private static final String LBL_DOMAIN_OBJECT = "Domain object:";
	private static final String SHELL_TITLE = "Create new update form";

	private final DomainObject domainObject;
	private final FormType formType;

	/**
	 * Constructor
	 * @param bot
	 * @param domainObject
	 * @param formType
	 */
	public CreateSingleRecordFormDialog(SWTWorkbenchBot bot, DomainObject domainObject, FormType formType) {
		super(bot, SHELL_TITLE);

		this.domainObject = domainObject;
		this.formType = formType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	@SuppressWarnings("unchecked")
	public void enterData() {
		selectProposalItem(LBL_DOMAIN_OBJECT, domainObject.getName());
		bot.comboBoxWithLabel(LBL_FORM_TYPE).setSelection(formType.name());

		// There are two 'OK' buttons. Just click the second one!
		final Matcher<Button> matcher = allOf(widgetOfType(Button.class), withMnemonic(CMD_OK));
		final var buttonWidgets = bot.widgets(matcher);
		boolean first = true;

		for (final var buttonWidget : buttonWidgets) {
			final var button = new SWTBotButton(buttonWidget);

			if (first) {
				first = false;
				continue;
			}

			button.click();
		}
	}

}
