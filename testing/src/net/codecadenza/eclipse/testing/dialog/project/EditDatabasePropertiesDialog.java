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
package net.codecadenza.eclipse.testing.dialog.project;

import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import net.codecadenza.eclipse.testing.domain.DataSource;
import net.codecadenza.eclipse.testing.domain.DatabaseVendor;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;

/**
 * <p>
 * Dialog for editing the database properties
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditDatabasePropertiesDialog extends AbstractDialog {
	private static final String SHELL_TITLE = "Edit database properties";
	private static final String LBL_CATALOG_NAME = "Catalog name:";
	private static final String LBL_SCHEMA_NAME = "Schema name:";

	private final DataSource dataSource;

	/**
	 * Constructor
	 * @param bot
	 * @param dataSource
	 */
	public EditDatabasePropertiesDialog(SWTWorkbenchBot bot, DataSource dataSource) {
		super(bot, SHELL_TITLE);

		this.dataSource = dataSource;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		if (dataSource.getDatabaseVendor() == DatabaseVendor.MYSQL) {
			final var txtCatalogName = bot.textWithLabel(LBL_CATALOG_NAME);
			txtCatalogName.setText(dataSource.getSchemaName());
		}
		else {
			final var txtSchemaName = bot.textWithLabel(LBL_SCHEMA_NAME);
			txtSchemaName.setText(dataSource.getSchemaName());
		}

		bot.button(CMD_OK).click();
	}

}
