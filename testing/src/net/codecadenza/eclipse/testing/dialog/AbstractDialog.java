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
package net.codecadenza.eclipse.testing.dialog;

import net.codecadenza.eclipse.testing.bots.AbstractBot;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.swt.finder.keyboard.Keystrokes;

/**
 * <p>
 * Base class for testing dialogs and wizards
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractDialog extends AbstractBot {

	public enum OperationMode {
		NOT_SET, ADD, EDIT, REMOVE, FULL
	}

	protected OperationMode operationMode;

	/**
	 * Constructor
	 * @param bot
	 * @param title
	 * @param operationMode
	 */
	protected AbstractDialog(SWTWorkbenchBot bot, String title, OperationMode operationMode) {
		super(bot);

		this.operationMode = operationMode;

		if (title != null)
			activateShellWithTitle(title);
	}

	/**
	 * Constructor
	 * @param bot
	 * @param title
	 */
	protected AbstractDialog(SWTWorkbenchBot bot, String title) {
		this(bot, title, OperationMode.NOT_SET);
	}

	/**
	 * An implementation must define what needs to be done after the dialog has been opened
	 */
	public abstract void enterData();

	/**
	 * Select the given item in the respective proposal text field
	 * @param label
	 * @param item
	 */
	protected void selectProposalItem(String label, String item) {
		final var txtProposal = bot.textWithLabel(label);
		txtProposal.setFocus();

		for (final char character : item.toCharArray())
			txtProposal.pressShortcut(Keystrokes.create(character));

		txtProposal.pressShortcut(Keystrokes.CR);
	}

}
