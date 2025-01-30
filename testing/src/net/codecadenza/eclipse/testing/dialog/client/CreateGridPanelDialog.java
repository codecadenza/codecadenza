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

import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import net.codecadenza.eclipse.testing.domain.DomainAssociation;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;

/**
 * <p>
 * Dialog for creating grid panels
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateGridPanelDialog extends AbstractDialog {
	private static final String LBL_ASSOCIATION = "Association:";
	private static final String LBL_DOMAIN_OBJECT = "Domain object:";
	private static final String SHELL_TITLE = "Create new grid panel";

	private final String domainObjectName;
	private final DomainAssociation association;

	/**
	 * Constructor
	 * @param bot
	 * @param domainObjectName
	 * @param association
	 */
	public CreateGridPanelDialog(SWTWorkbenchBot bot, String domainObjectName, DomainAssociation association) {
		super(bot, SHELL_TITLE);

		this.domainObjectName = domainObjectName;
		this.association = association;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		var itemText = "Collection<" + association.getTarget().getName() + ">" + association.getName();
		itemText += " [" + association.getType() + "]";

		selectProposalItem(LBL_DOMAIN_OBJECT, domainObjectName);

		bot.comboBoxWithLabel(LBL_ASSOCIATION).setSelection(itemText);
		bot.button(CMD_OK).click();
	}

}
