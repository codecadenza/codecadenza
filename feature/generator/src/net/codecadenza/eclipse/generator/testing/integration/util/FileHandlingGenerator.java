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
package net.codecadenza.eclipse.generator.testing.integration.util;

import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;

/**
 * <p>
 * Generator for file handling
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FileHandlingGenerator {
	public static final String FILE_SERVICE_NAME = "fileService";
	public static final String UPLOAD_FILE_PATH = "pathInBackend";
	public static final String DOWNLOAD_FILE_CONSTANT = "DOWNLOAD_FILE_NAME";

	private final IntegrationMethodTestInvocation methodInvocation;
	private final IntegrationTechnology integrationTechnology;

	/**
	 * Constructor
	 * @param methodInvocation
	 */
	public FileHandlingGenerator(IntegrationMethodTestInvocation methodInvocation) {
		final AbstractIntegrationBean integrationBean = methodInvocation.getIntegrationMethod().getIntegrationBean();

		this.methodInvocation = methodInvocation;
		this.integrationTechnology = integrationBean.getIntegrationTechnology();
	}

	/**
	 * Create the fragment for uploading a file
	 * @return the generated content
	 */
	public String addFileUpload() {
		final BoundaryMethodTypeEnumeration methodType = methodInvocation.getIntegrationMethod().getBoundaryMethod().getMethodType();

		if (methodType == BoundaryMethodTypeEnumeration.UPLOAD || methodType == BoundaryMethodTypeEnumeration.UPLOAD_IMPORT)
			return addDirectFileUpload();
		else
			return addIndirectFileUpload();
	}

	/**
	 * Create the fragment for downloading a file
	 * @return the generated content
	 */
	public String addFileDownload() {
		final var b = new StringBuilder();
		b.append("\n");
		b.append("final Path tempFilePath = testDir.resolve(" + DOWNLOAD_FILE_CONSTANT + ");\n");

		if (integrationTechnology == IntegrationTechnology.SOAP) {
			b.append("final var dataHandler = " + FILE_SERVICE_NAME + ".downloadFile(actualResultObject);\n\n");
			b.append("try(final InputStream inputStream = dataHandler.getInputStream())\n");
			b.append("{\n");
			b.append("Files.copy(inputStream, tempFilePath, StandardCopyOption.REPLACE_EXISTING);\n");
			b.append("}\n");
			b.append("catch (IOException e)\n");
			b.append("{\n");
			b.append("throw new UncheckedIOException(e);\n");
			b.append("}\n");
		}

		b.append("\ntry\n");
		b.append("{\n");

		if (integrationTechnology == IntegrationTechnology.REST || integrationTechnology == IntegrationTechnology.KAFKA)
			b.append(FILE_SERVICE_NAME + ".downloadFile(actualResultObject, tempFilePath.toFile());\n\n");
		else if (integrationTechnology == IntegrationTechnology.JMS)
			b.append(FILE_SERVICE_NAME + ".downloadFile(actualResultObject, tempFilePath.toFile(), Duration.ofSeconds(1));\n\n");
		else if (integrationTechnology == IntegrationTechnology.RMI) {
			b.append("final byte[] content = " + FILE_SERVICE_NAME + ".downloadFile(actualResultObject);\n");
			b.append("Files.write(tempFilePath, content);\n\n");
		}

		b.append("assertThat(tempFilePath).exists();\n");
		b.append("assertThat(Files.size(tempFilePath)).isNotZero();\n");
		b.append("}\n");
		b.append("catch (IOException e)\n");
		b.append("{\n");
		b.append("throw new UncheckedIOException(e);\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * Create the fragment for an indirect upload operation
	 * @return the generated content
	 */
	private String addIndirectFileUpload() {
		final var b = new StringBuilder();
		final TestDataAttribute filePathAttribute = methodInvocation.getFilePathAttribute();
		final TestDataAttribute fileNameAttribute = methodInvocation.getFileNameAttribute();
		final String objectParameterName = methodInvocation.getParameters().getFirst().getName();
		final String fileName;

		if (fileNameAttribute != null)
			fileName = "object." + fileNameAttribute.getMappingAttribute().getGetterName();
		else
			fileName = "fileToUpload.getName()";

		b.append("\n");
		b.append("final File fileToUpload = new File(" + objectParameterName);
		b.append("." + filePathAttribute.getMappingAttribute().getGetterName() + ");\n\n");
		b.append("try\n");
		b.append("{\n");

		if (integrationTechnology == IntegrationTechnology.SOAP || integrationTechnology == IntegrationTechnology.RMI) {
			b.append("final byte[] content = Files.readAllBytes(fileToUpload.toPath());\n");
			b.append("final String pathInBackend = " + FILE_SERVICE_NAME);
			b.append(".uploadFile(" + fileName + ", content);\n\n");
		}
		else {
			b.append("final String pathInBackend = " + FILE_SERVICE_NAME);
			b.append(".uploadFile(fileToUpload");

			if (integrationTechnology == IntegrationTechnology.JMS)
				b.append(", Duration.ofSeconds(1)");

			b.append(");\n\n");
		}

		b.append(objectParameterName + "." + filePathAttribute.getMappingAttribute().getSetterName() + "(pathInBackend);\n");
		b.append("}\n");
		b.append("catch (Exception e)\n");
		b.append("{\n");
		b.append("throw new IllegalStateException(e);\n");
		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * Create the fragment for a direct upload operation
	 * @return the generated content
	 */
	private String addDirectFileUpload() {
		final var b = new StringBuilder();
		final String fileParameterName = methodInvocation.getParameters().getLast().getName();

		b.append("\n");
		b.append("final File fileToUpload = new File(" + fileParameterName + ");\n");
		b.append("final String " + UPLOAD_FILE_PATH + ";\n\n");
		b.append("try\n");
		b.append("{\n");

		if (integrationTechnology == IntegrationTechnology.SOAP || integrationTechnology == IntegrationTechnology.REST) {
			b.append("final byte[] content = Files.readAllBytes(fileToUpload.toPath());\n\n");
			b.append(UPLOAD_FILE_PATH + " = " + FILE_SERVICE_NAME + ".uploadFile(fileToUpload.getName(), content);\n");
		}
		else {
			b.append(UPLOAD_FILE_PATH + " = " + FILE_SERVICE_NAME + ".uploadFile(fileToUpload");

			if (integrationTechnology == IntegrationTechnology.JMS)
				b.append(", Duration.ofSeconds(1)");

			b.append(");\n");
		}

		b.append("}\n");
		b.append("catch(Exception e)\n");
		b.append("{\n");
		b.append("throw new IllegalStateException(e);\n");
		b.append("}\n\n");

		return b.toString();
	}
}
