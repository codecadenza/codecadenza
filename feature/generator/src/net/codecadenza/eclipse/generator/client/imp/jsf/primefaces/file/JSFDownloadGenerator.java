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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.file;

import java.util.List;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.domain.DomainAttribute;

/**
 * <p>
 * Generator for file download operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFDownloadGenerator {
	private final AbstractJavaSourceGenerator generator;
	private final String managedBeanName;
	private final List<FormAction> actions;

	/**
	 * Constructor
	 * @param generator
	 * @param form
	 */
	public JSFDownloadGenerator(AbstractJavaSourceGenerator generator, Form form) {
		this(generator, JSFGeneratorUtil.createManagedBeanName(form.getName()), form.getActions());
	}

	/**
	 * Constructor
	 * @param generator
	 * @param panel
	 */
	public JSFDownloadGenerator(AbstractJavaSourceGenerator generator, FormPanel panel) {
		this(generator, JSFGeneratorUtil.createManagedBeanName(panel.getName()), panel.getActions());
	}

	/**
	 * Constructor
	 * @param generator
	 * @param managedBeanName
	 * @param actions
	 */
	public JSFDownloadGenerator(AbstractJavaSourceGenerator generator, String managedBeanName, List<FormAction> actions) {
		this.generator = generator;
		this.managedBeanName = managedBeanName;
		this.actions = actions.stream().filter(e -> e.getType() == ActionType.DOWNLOAD).toList();
	}

	/**
	 * Add necessary imports
	 */
	public void addImports() {
		if (!actions.isEmpty()) {
			generator.importPackage("org.primefaces.model");
			generator.importPackage("java.io");
			generator.importPackage("net.codecadenza.runtime.webclient.primefaces.util");
			generator.importClass("jakarta.faces.application.FacesMessage");
		}
	}

	/**
	 * @return the generated content
	 */
	public String createCommandFragment() {
		final var b = new StringBuilder();

		actions.stream().map(action -> action.getBoundaryMethod().getDomainAttribute()).forEach(downloadAttr -> {
			final var downloadEL = managedBeanName + "." + downloadAttr.getName();
			final var buttonId = "cmd" + downloadAttr.getUpperCaseName() + "Download";

			b.append("\t\t<p:commandButton id=\"" + buttonId + "\" value=\"#{i18n.action_download}\" ajax=\"false\">\n");
			b.append("\t\t\t<p:fileDownload value=\"#{" + downloadEL + "}\"/>\n");
			b.append("\t\t</p:commandButton>\n");
		});

		return b.toString();
	}

	/**
	 * @param formAction
	 * @param selItemName
	 */
	public void addDownloadMethod(FormAction formAction, String selItemName) {
		final DomainAttribute attr = formAction.getBoundaryMethod().getDomainAttribute();
		final var methodSignature = "StreamedContent " + attr.getGetterName();
		final var b = new StringBuilder();

		b.append("/**\n");
		b.append(" * @return the download stream that contains the file content\n");
		b.append(" */\n");
		b.append("@SuppressWarnings(\"resource\")\n");
		b.append(generator.getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");
		b.append("final String path = ");

		new ServiceInvocationGenerator(formAction.getBoundaryMethod(), b).addInvocation(selItemName);

		b.append("\n");
		b.append("if(path == null || path.isEmpty())\n");
		b.append("return null;\n\n");

		generator.addDebugLog(b, "Download file '{}'", "path");

		b.append("\n");
		b.append("final String fileName = new File(path).getName();\n");
		b.append("final var inputStream = new FileInputStream(path);\n\n");
		b.append("return DefaultStreamedContent.builder().name(fileName).stream(() -> inputStream).build();\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		generator.addErrorLog(b, "Error while performing download operation!", "e");

		b.append("\n");
		b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_DOWNLOAD_FAIL, e);\n");
		b.append("return null;\n");
		b.append("}\n");
		b.append("}\n\n");

		generator.addMethod(methodSignature, b.toString());
	}

	/**
	 * @param selItemName
	 */
	public void addDownloadMethods(String selItemName) {
		// Add methods for all file download actions
		actions.forEach(action -> addDownloadMethod(action, selItemName));
	}

}
