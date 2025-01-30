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
package net.codecadenza.eclipse.generator.service;

import static net.codecadenza.eclipse.shared.Constants.UTF_8;

import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for logging configuration files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class LoggingConfigurationGenerator {
	private final BuildArtifact buildArtifact;

	/**
	 * Constructor
	 * @param buildArtifact
	 */
	public LoggingConfigurationGenerator(BuildArtifact buildArtifact) {
		this.buildArtifact = buildArtifact;
	}

	/**
	 * Create the content of the file
	 * @return the generated content
	 */
	public String createContent() {
		final var b = new StringBuilder();
		final Project project = buildArtifact.getProject();
		final var logFileName = project.getCode().toLowerCase() + "_" + buildArtifact.getType().getName().toLowerCase();

		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<Configuration>\n\n");
		b.append("\t<Appenders>\n");
		b.append("\t\t<RollingFile name=\"FILE\" fileName=\"${sys:user.home}/development/test/logs/" + logFileName + ".log\" ");
		b.append("filePattern=\"${sys:user.home}/development/test/logs/" + logFileName + "-%i.log\">\n");
		b.append("\t\t\t<PatternLayout pattern=\"%d{dd.MM.yyyy HH:mm:ss,SSS} %-5p %c (%L) - %m%n\"/>\n\n");
		b.append("\t\t\t<Policies>\n");
		b.append("\t\t\t\t<!-- Define the maximum size of a log file -->\n");
		b.append("\t\t\t\t<SizeBasedTriggeringPolicy size=\"1 MB\"/>\n");
		b.append("\t\t\t</Policies>\n\n");
		b.append("\t\t\t<!-- Define the maximum number of files to be created -->\n");
		b.append("\t\t\t<DefaultRolloverStrategy max=\"10\"/>\n");
		b.append("\t\t</RollingFile>\n\n");
		b.append("\t\t<Console name=\"STDOUT\" target=\"SYSTEM_OUT\">\n");
		b.append("\t\t\t<PatternLayout pattern=\"%d{dd.MM.yyyy HH:mm:ss,SSS} %-5p %c (%L) - %m%n\"/>\n");
		b.append("\t\t</Console>\n");
		b.append("\t</Appenders>\n\n");
		b.append("\t<Loggers>\n");
		b.append("\t\t<Root level=\"INFO\">\n");
		b.append("\t\t\t<AppenderRef ref=\"FILE\"/>\n");
		b.append("\t\t\t<AppenderRef ref=\"STDOUT\"/>\n");
		b.append("\t\t</Root>\n\n");
		b.append("\t\t<Logger name=\"" + project.getRootNamespace().toString() + "\" level=\"DEBUG\"/>\n");
		b.append("\t\t<Logger name=\"net.codecadenza.runtime\" level=\"DEBUG\"/>\n");

		if (buildArtifact.getType() == BuildArtifactType.INTEGRATION_CLIENT_KAFKA) {
			b.append("\t\t<Logger name=\"org.apache.kafka.clients\" level=\"WARN\"/>\n");
			b.append("\t\t<Logger name=\"org.apache.kafka.common\" level=\"WARN\"/>\n");
		}
		else if (buildArtifact.getType() == BuildArtifactType.SERVER && project.isSpringBootApplication()) {
			if (project.getIntegrationModuleByArtifact(BuildArtifactType.INTEGRATION_IMP_KAFKA) != null) {
				b.append("\t\t<Logger name=\"org.apache.kafka\" level=\"WARN\"/>\n");
				b.append("\t\t<Logger name=\"org.springframework.kafka.listener\" level=\"WARN\"/>\n");
			}

			b.append("\t\t<Logger name=\"com.zaxxer.hikari\" level=\"WARN\"/>\n");
		}

		b.append("\t</Loggers>\n\n");
		b.append("</Configuration>\n");

		return b.toString();
	}

}
