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
package net.codecadenza.eclipse.testing.dialog.exchange;

import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import net.codecadenza.eclipse.testing.domain.DomainAttribute;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;

/**
 * <p>
 * Dialog for creating a data exchange element
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateExchangeElementDialog extends AbstractDialog {
	private static final String ATTRIBUTE_NAME = "simpleElement";
	private static final String LBL_NAME = "Name:";
	private static final String LBL_TYPE = "Mapping type:";
	private static final String LBL_MAPPING_NAME = "Mapping attribute name:";
	private static final String MAPPED_ATTRIBUTE_NAME = "mappedElement";
	private static final String SHELL_TITLE = "Create new data exchange element";

	private final boolean mapped;

	/**
	 * Constructor
	 * @param bot
	 * @param mapped
	 */
	public CreateExchangeElementDialog(SWTWorkbenchBot bot, boolean mapped) {
		super(bot, SHELL_TITLE);

		this.mapped = mapped;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		if (mapped) {
			bot.textWithLabel(LBL_NAME).setText(MAPPED_ATTRIBUTE_NAME);
			bot.textWithLabel(LBL_MAPPING_NAME).setText(MAPPED_ATTRIBUTE_NAME);

			selectProposalItem(LBL_TYPE, DomainAttribute.TYPE_LONG);
		}
		else {
			bot.textWithLabel(LBL_NAME).setText(ATTRIBUTE_NAME);

			selectProposalItem(LBL_TYPE, DomainAttribute.TYPE_STRING);
		}

		bot.button(CMD_OK).click();
	}

}
