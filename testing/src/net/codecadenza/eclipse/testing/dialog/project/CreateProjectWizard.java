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
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.domain.TechnologyPlatform;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;

/**
 * <p>
 * Wizard for creating a new CodeCadenza project
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateProjectWizard extends AbstractDialog {
	private static final String LBL_DB_VENDOR = "Database vendor:";
	private static final String LBL_PLATFORM = "Platform:";
	private static final String LBL_IMPLEMENTATION = "Implementation:";
	private static final String LBL_PROJECT_NAME = "Project name:";
	private static final String LBL_MODULE_JMS = "Add JMS module";
	private static final String LBL_MODULE_KAFKA = "Add Kafka module";
	private static final String LBL_MODULE_REST = "Add REST module";
	private static final String LBL_MODULE_RMI = "Add RMI module";
	private static final String LBL_MODULE_SOAP = "Add SOAP module";
	private static final String SERVER_PLATFORM_GLASSFISH = "GLASSFISH";
	private static final String SERVER_PLATFORM_NONE = "NONE";
	private static final String SERVER_PLATFORM_TOMCAT = "TOMCAT";
	private static final String SHELL_TITLE = "Create new CodeCadenza project";

	private final Project project;

	/**
	 * Constructor
	 * @param bot
	 * @param project
	 */
	public CreateProjectWizard(SWTWorkbenchBot bot, Project project) {
		super(bot, SHELL_TITLE);

		this.project = project;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		bot.textWithLabel(LBL_PROJECT_NAME).setText(project.getName());
		bot.checkBox().select();
		bot.button(CMD_NEXT).click();
		bot.comboBoxWithLabel(LBL_DB_VENDOR).setSelection(project.getDataSource().getDatabaseVendor().name());
		bot.button(CMD_NEXT).click();
		bot.comboBoxWithLabel(LBL_IMPLEMENTATION).setSelection(project.getClientPlatform().name());

		if (project.getTechnologyPlatform() == TechnologyPlatform.JAKARTA_EE)
			bot.comboBoxWithLabel(LBL_PLATFORM).setSelection(SERVER_PLATFORM_GLASSFISH);
		else if (project.getTechnologyPlatform() == TechnologyPlatform.SPRING_BOOT)
			bot.comboBoxWithLabel(LBL_PLATFORM).setSelection(SERVER_PLATFORM_TOMCAT);
		else
			bot.comboBoxWithLabel(LBL_PLATFORM).setSelection(SERVER_PLATFORM_NONE);

		bot.button(CMD_NEXT).click();

		// Activate all available integration modules
		final var chkSOAPModule = bot.checkBox(LBL_MODULE_SOAP);

		if (chkSOAPModule.isEnabled())
			chkSOAPModule.select();

		final var chkRESTModule = bot.checkBox(LBL_MODULE_REST);

		if (chkRESTModule.isEnabled())
			chkRESTModule.select();

		final var chkRMIModule = bot.checkBox(LBL_MODULE_RMI);

		if (chkRMIModule.isEnabled())
			chkRMIModule.select();

		final var chkKafkaModule = bot.checkBox(LBL_MODULE_KAFKA);

		if (chkKafkaModule.isEnabled())
			chkKafkaModule.select();

		final var chkJMSModule = bot.checkBox(LBL_MODULE_JMS);

		if (chkJMSModule.isEnabled())
			chkJMSModule.select();

		bot.button(CMD_NEXT).click();
		bot.button(CMD_NEXT).click();
		bot.button(CMD_FINISH).click();
	}

}
