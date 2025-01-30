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
 * Dialog to edit the project's data source
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditDataSourceDialog extends AbstractDialog {
	private static final String SHELL_TITLE = "Edit data source";
	private static final String DB_INSTANCE = "[instance]";
	private static final String LBL_CONNECTION_URL = "Connection URL:";

	private final DataSource dataSource;

	/**
	 * Constructor
	 * @param bot
	 * @param dataSource
	 */
	public EditDataSourceDialog(SWTWorkbenchBot bot, DataSource dataSource) {
		super(bot, SHELL_TITLE);

		this.dataSource = dataSource;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		final var txtConnectionURL = bot.textWithLabel(LBL_CONNECTION_URL);
		final var jdbcURL = txtConnectionURL.getText();

		if (dataSource.getDatabaseVendor() == DatabaseVendor.MYSQL)
			txtConnectionURL.setText(jdbcURL.replace(DB_INSTANCE, dataSource.getSchemaName()));
		else
			txtConnectionURL.setText(jdbcURL.replace(DB_INSTANCE, dataSource.getDatabaseName()));

		bot.button(CMD_OK).click();
	}

}
