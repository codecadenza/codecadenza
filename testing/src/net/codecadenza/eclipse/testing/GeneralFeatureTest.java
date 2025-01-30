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
package net.codecadenza.eclipse.testing;

import net.codecadenza.eclipse.testing.bots.GeneralFeatureBot;
import net.codecadenza.eclipse.testing.bots.WorkspaceManagerBot;
import net.codecadenza.eclipse.testing.domain.ClientPlatform;
import net.codecadenza.eclipse.testing.domain.ProjectType;
import net.codecadenza.eclipse.testing.domain.TechnologyPlatform;
import org.eclipse.swtbot.eclipse.gef.finder.SWTGefBot;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * <p>
 * Test the basic functionality of the UI plug-in and the domain diagram editor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class GeneralFeatureTest {
	private static final SWTGefBot bot = new SWTGefBot();

	/**
	 * Initialize the test
	 */
	@BeforeAll
	static void initTest() {
		new WorkspaceManagerBot(bot).prepareWorkspace();
	}

	/**
	 * Test general features of the UI plug-in and the domain diagram editor with different client and technology platforms
	 * @param technologyPlatformName
	 * @param clientPlatformName
	 */
	@ParameterizedTest
	@CsvSource({ "JAKARTA_EE,JSF_PRIMEFACES", "JAVA_SE,JAVAFX", "SPRING_BOOT,VAADIN" })
	void testGeneralFeatures(String technologyPlatformName, String clientPlatformName) {
		final var technologyPlatform = TechnologyPlatform.valueOf(technologyPlatformName);
		final var clientPlatform = ClientPlatform.valueOf(clientPlatformName);

		for (final var projectType : ProjectType.values())
			new GeneralFeatureBot(bot).performGeneralTest(projectType, technologyPlatform, clientPlatform);
	}

}
