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
package net.codecadenza.eclipse.testing.dialog.domain;

import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.allOf;
import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.widgetOfType;
import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.withMnemonic;

import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotCheckBox;

/**
 * <p>
 * Dialog for creating a one-to-many association
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateOneToManyAssociationDialog extends AbstractDialog {
	private static final String SHELL_TITLE = "Create new one-to-many association";

	private final boolean bidirectional;

	/**
	 * Constructor
	 * @param bot
	 * @param bidirectional
	 */
	public CreateOneToManyAssociationDialog(SWTWorkbenchBot bot, boolean bidirectional) {
		super(bot, SHELL_TITLE);

		this.bidirectional = bidirectional;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	@SuppressWarnings("unchecked")
	public void enterData() {
		if (!bidirectional) {
			final var buttonWidgets = bot.widgets(allOf(widgetOfType(Button.class), withMnemonic("")));

			// The first button represents the checkbox for defining if the association should be either bidirectional or unidirectional
			final SWTBotCheckBox checkBox = new SWTBotCheckBox(buttonWidgets.get(0));
			checkBox.deselect();
		}

		bot.button(CMD_OK).click();
	}

}
