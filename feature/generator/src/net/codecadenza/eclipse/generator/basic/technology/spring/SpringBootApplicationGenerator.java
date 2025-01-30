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
package net.codecadenza.eclipse.generator.basic.technology.spring;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the main class of a Spring Boot application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SpringBootApplicationGenerator extends AbstractJavaSourceGenerator {
	private final Project project;
	private final BuildArtifactType artifactType;

	/**
	 * Constructor
	 * @param project
	 */
	public SpringBootApplicationGenerator(Project project) {
		this.project = project;

		if (project.hasJSFOrVaadinClient())
			this.artifactType = BuildArtifactType.GUI;
		else
			this.artifactType = BuildArtifactType.SERVER;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final String packageName = project.getRootNamespace().toString();

		final var javaFile = new JavaFile(project, artifactType, "Application", packageName);
		javaFile.setComment("Main class of this Spring Boot application");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importClass("org.springframework.boot.SpringApplication");
		importClass("org.springframework.boot.autoconfigure.SpringBootApplication");
		importClass("org.springframework.boot.autoconfigure.domain.EntityScan");
		importClass("org.springframework.boot.web.servlet.support.SpringBootServletInitializer");
		importClass("org.springframework.context.annotation.Bean");
		importClass("org.springframework.context.annotation.ComponentScan");
		importClass("org.springframework.scheduling.annotation.EnableScheduling");
		importClass("org.springframework.security.web.firewall.StrictHttpFirewall");

		if (project.hasEclipseClient() || project.hasJavaFXClient() || project.hasSwingClient())
			importClass("org.springframework.boot.web.servlet.ServletComponentScan");

		if (project.hasVaadinClient()) {
			importPackage("com.vaadin.flow.component.dependency");
			importPackage("com.vaadin.flow.component.page");
			importPackage("com.vaadin.flow.server");
			importPackage("com.vaadin.flow.theme");
			importClass("org.springframework.web.context.annotation.SessionScope");
			importPackage("net.codecadenza.runtime.webclient.vaadin.util");
			importPackage("net.codecadenza.runtime.webclient.vaadin.i18n");
		}
		else if (project.hasJSFClient()) {
			importPackage("net.codecadenza.runtime.webclient.primefaces.filter");
			importPackage("org.springframework.boot.web.server");
			importPackage("org.springframework.boot.web.servlet");
			importPackage("org.springframework.core");
			importPackage("org.springframework.http");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@SpringBootApplication\n");
		b.append("@EnableScheduling");
		b.append("@ComponentScan(\"net.codecadenza.runtime.server\")\n");
		b.append("@ComponentScan(\"" + project.getRootNamespace().toString() + "\")\n");

		if (project.hasJSFClient())
			b.append("@ComponentScan(\"net.codecadenza.runtime.webclient.primefaces.util\")\n");

		if (project.getIntegrationModuleByArtifact(BuildArtifactType.INTEGRATION_IMP_KAFKA) != null)
			b.append("@ComponentScan(\"net.codecadenza.runtime.spring.kafka\")\n");

		if (project.hasEclipseClient() || project.hasJavaFXClient() || project.hasSwingClient())
			b.append("@ServletComponentScan(\"net.codecadenza.runtime.server.transport\")\n");

		b.append("@EntityScan(\"" + project.getDomainNamespace().toString() + "\")\n");

		if (project.hasVaadinClient()) {
			b.append("@PWA(name = \"My generated Application\", shortName=\"My App\")\n");
			b.append("@Theme(\"codecadenza\")\n");
			b.append("@CssImport(value=\"./themes/codecadenza/app-layout.css\", themeFor=\"vaadin-app-layout\")\n");
			b.append("@CssImport(value=\"./themes/codecadenza/check-box.css\", themeFor=\"vaadin-checkbox\")\n");
			b.append("@CssImport(value=\"./themes/codecadenza/form-item.css\", themeFor=\"vaadin-form-item\")\n");
			b.append("@CssImport(value=\"./themes/codecadenza/text-area.css\", themeFor=\"vaadin-text-area\")\n");
			b.append("@CssImport(value=\"./themes/codecadenza/text-field.css\", themeFor=\"vaadin-text-field\")\n");
			b.append("@CssImport(value=\"./themes/codecadenza/title-area-dialog-shadow.css\", themeFor=\"vaadin-dialog-overlay\")\n");
			b.append("@CssImport(value=\"./themes/codecadenza/title-area-dialog-dom.css\")\n");
			b.append("@NpmPackage(value=\"lumo-css-framework\", version=\"3.0.14\")\n");
		}

		b.append("public class Application extends SpringBootServletInitializer");

		if (project.hasVaadinClient())
			b.append(" implements AppShellConfigurator");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		if (project.hasJSFClient()) {
			addPrivateConstant(JavaType.STRING, "ERROR_PAGE", "\"/error.jsf\"").create();
			addPrivateConstant(JavaType.STRING, "NOT_FOUND_PAGE", "\"/view/index.jsf\"").create();
		}
		else if (project.hasVaadinClient())
			addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var b = new StringBuilder();
		var methodSignature = "void main(String[] args)";

		b.append("/**\n");
		b.append(" * Start the application\n");
		b.append(" * @param args\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public static " + methodSignature + "\n");
		b.append("{\n");
		b.append("SpringApplication.run(Application.class, args);\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		b = new StringBuilder();
		methodSignature = "StrictHttpFirewall httpFirewall()";

		b.append("/**\n");
		b.append(" * @return a {@link StrictHttpFirewall} instance\n");
		b.append(" */\n");
		b.append("@Bean\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("final var firewall = new StrictHttpFirewall();\n\n");
		b.append("// Some REST methods require that the URL contains an encoded percentage!\n");
		b.append("firewall.setAllowUrlEncodedPercent(true);\n\n");
		b.append("return firewall;\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		if (project.hasVaadinClient())
			addVaadinMethods();
		else if (project.hasJSFClient())
			addJSFMethods();
	}

	/**
	 * Add JSF-specific methods
	 */
	private void addJSFMethods() {
		var b = new StringBuilder();
		var methodSignature = "FilterRegistrationBean<CharacterEncodingFilter> getCharacterEncodingFilter()";

		b.append("/**\n");
		b.append(" * Register the character encoding filter\n");
		b.append(" * @return the respective filter registration bean\n");
		b.append(" */\n");
		b.append("@Bean\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("final var filterRegBean = new FilterRegistrationBean<CharacterEncodingFilter>();\n");
		b.append("filterRegBean.setFilter(new CharacterEncodingFilter());\n");
		b.append("filterRegBean.setOrder(Ordered.LOWEST_PRECEDENCE);\n\n");
		b.append("return filterRegBean;\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		b = new StringBuilder();
		methodSignature = "ErrorPageRegistrar getErrorPageRegistrar()";

		b.append("/**\n");
		b.append(" * @return an object that controls how different errors should be handled\n");
		b.append(" */\n");
		b.append("@Bean\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return registry ->\n");
		b.append("{\n");
		b.append("registry.addErrorPages(new ErrorPage(Throwable.class, ERROR_PAGE));\n");
		b.append("registry.addErrorPages(new ErrorPage(HttpStatus.NOT_FOUND, NOT_FOUND_PAGE));\n");
		b.append("registry.addErrorPages(new ErrorPage(HttpStatus.INTERNAL_SERVER_ERROR, ERROR_PAGE));\n");
		b.append("};\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Add Vaadin-specific methods
	 */
	private void addVaadinMethods() {
		var b = new StringBuilder();
		var methodSignature = "PreferencesStore getPreferencesStore()";

		b.append("/**\n");
		b.append(" * @return an instance of class {@link PreferencesStore}\n");
		b.append(" */\n");
		b.append("@Bean\n");
		b.append("@SessionScope\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return new PreferencesStore(getCookieManager(), getSessionStore());\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		b = new StringBuilder();
		methodSignature = "SessionStore getSessionStore()";

		b.append("/**\n");
		b.append(" * @return an instance of class {@link SessionStore}\n");
		b.append(" */\n");
		b.append("@Bean\n");
		b.append("@SessionScope\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return new SessionStore();\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		b = new StringBuilder();
		methodSignature = "CookieManager getCookieManager()";

		b.append("/**\n");
		b.append(" * @return an instance of class {@link CookieManager}\n");
		b.append(" */\n");
		b.append("@Bean\n");
		b.append("@SessionScope\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return new CookieManager();\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		b = new StringBuilder();
		methodSignature = "I18NService getI18NService()";

		b.append("/**\n");
		b.append(" * @return an instance of class {@link I18NService}\n");
		b.append(" */\n");
		b.append("@Bean\n");
		b.append("@SessionScope\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return new I18NService(getPreferencesStore());\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}
}
