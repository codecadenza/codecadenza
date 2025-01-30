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
package net.codecadenza.runtime.server.mail;

import jakarta.activation.DataHandler;
import jakarta.activation.FileDataSource;
import jakarta.mail.Message;
import jakarta.mail.MessagingException;
import jakarta.mail.Session;
import jakarta.mail.Transport;
import jakarta.mail.internet.InternetAddress;
import jakarta.mail.internet.MimeBodyPart;
import jakarta.mail.internet.MimeMessage;
import jakarta.mail.internet.MimeMultipart;
import java.io.FileOutputStream;
import java.lang.invoke.MethodHandles;
import java.util.Collection;
import java.util.Properties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Implementation of a simple email service
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MailService {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String SMTP_PROTOCOL = "smtp";
	private static final String TEMP_ATTACHMENT_FILE_NAME = "~TempAttachment-";

	private final String host;
	private String userName;
	private String password;
	private final String senderEmail;
	private final String senderName;

	/**
	 * Constructor to initialize mail service
	 * @param host
	 * @param senderEmail
	 * @param senderName
	 * @throws IllegalArgumentException if one of the provided parameter values is invalid
	 */
	public MailService(String host, String senderEmail, String senderName) {
		if (host == null || host.isEmpty())
			throw new IllegalArgumentException("Could not initialize service because host is not specified!");

		if (senderEmail == null || senderEmail.isEmpty())
			throw new IllegalArgumentException("Could not initialize service because sender is not specified!");

		this.host = host;
		this.senderEmail = senderEmail;
		this.senderName = senderName;
	}

	/**
	 * Constructor to initialize mail service with user name and password to perform simple log on to SMTP server
	 * @param host
	 * @param senderEmail
	 * @param senderName
	 * @param userName
	 * @param password
	 * @throws IllegalArgumentException if one of the provided parameter values is invalid
	 */
	public MailService(String host, String senderEmail, String senderName, String userName, String password) {
		this(host, senderEmail, senderName);

		this.userName = userName;
		this.password = password;
	}

	/**
	 * Send mail
	 * @param mailMessage
	 * @throws MailServiceException if sending of the mail has failed
	 */
	public void sendMail(MailMessage mailMessage) {
		Transport transport = null;

		// Check mail message data
		if (mailMessage.getTo() == null || mailMessage.getTo().isEmpty())
			throw new MailServiceException("Cannot send email because no receiver is specified!");

		for (final String to : mailMessage.getTo())
			if (to.isEmpty())
				throw new MailServiceException("Cannot send email because receiver address must not be empty!");

		if (mailMessage.getCc() != null)
			for (final String cc : mailMessage.getCc())
				if (cc.isEmpty())
					throw new MailServiceException("Cannot send email because receiver of carbon copy must not be empty!");

		try {
			// Set server properties
			final var props = new Properties();
			props.setProperty("mail.transport.protocol", SMTP_PROTOCOL);
			props.setProperty("mail.host", host);

			final Session mailSession = Session.getDefaultInstance(props, null);

			// Create a new message and set the content type
			final var message = new MimeMessage(mailSession);

			if (mailMessage.getSubject() != null)
				message.setSubject(mailMessage.getSubject());
			else
				message.setSubject("");

			// Add the recipients
			for (final String to : mailMessage.getTo())
				message.addRecipient(Message.RecipientType.TO, new InternetAddress(to));

			// Set the sender
			InternetAddress sender = null;

			if (senderName != null && !senderName.isEmpty())
				sender = new InternetAddress(senderEmail, senderName);
			else
				sender = new InternetAddress(senderEmail);

			message.setSender(sender);

			if (mailMessage.getFromEmail() != null && !mailMessage.getFromEmail().isEmpty()) {
				InternetAddress from = null;

				// The mail agent should send the message on behalf of someone else!
				if (mailMessage.getFromName() != null && !mailMessage.getFromName().isEmpty())
					from = new InternetAddress(mailMessage.getFromEmail(), mailMessage.getFromName());
				else
					from = new InternetAddress(mailMessage.getFromEmail());

				message.setFrom(from);
			}
			else
				message.setFrom(sender);

			// Add carbon copies
			if (mailMessage.getCc() != null)
				for (final String cc : mailMessage.getCc())
					message.addRecipient(Message.RecipientType.CC, new InternetAddress(cc));

			// Create the message part
			var messageBodyPart = new MimeBodyPart();

			// Set the message text
			if (mailMessage.getMessage() != null)
				messageBodyPart.setText(mailMessage.getMessage());
			else
				messageBodyPart.setText("");

			final var multipart = new MimeMultipart();
			multipart.addBodyPart(messageBodyPart);

			final String tempFileName = TEMP_ATTACHMENT_FILE_NAME + System.currentTimeMillis();

			// Part two is the attachment
			if (mailMessage.getAttachmentName() != null) {
				// Save the attached file to a temporary folder
				try (final var fos = new FileOutputStream(tempFileName)) {
					fos.write(mailMessage.getAttachmentContent());
				}

				messageBodyPart = new MimeBodyPart();

				final var source = new FileDataSource(tempFileName);

				messageBodyPart.setDataHandler(new DataHandler(source));
				messageBodyPart.setFileName(mailMessage.getAttachmentName());

				multipart.addBodyPart(messageBodyPart);
			}

			// Set the message content
			message.setContent(multipart);

			// Connect to the SMTP server and send the mail!
			transport = mailSession.getTransport();

			if (userName != null && password != null)
				transport.connect(host, userName, password);
			else
				transport.connect();

			transport.sendMessage(message, message.getRecipients(Message.RecipientType.TO));

			if (mailMessage.getCc() != null && !mailMessage.getCc().isEmpty())
				transport.sendMessage(message, message.getRecipients(Message.RecipientType.CC));
		}
		catch (final Exception e) {
			logger.error("Error while sending email message!", e);

			throw new MailServiceException(e.getMessage());
		}
		finally {
			try {
				if (transport != null)
					transport.close();
			}
			catch (final MessagingException e) {
				logger.error("Could not close email transport service!", e);
			}
		}
	}

	/**
	 * Send email
	 * @param to
	 * @param subject
	 * @param fromEmail
	 * @param fromName
	 * @throws MailServiceException if sending of the mail has failed
	 */
	public void sendMail(String to, String subject, String fromEmail, String fromName) {
		final var mailMessage = new MailMessage();

		// Initialize the message object
		mailMessage.getTo().add(to);
		mailMessage.setSubject(subject);
		mailMessage.setFromEmail(fromEmail);
		mailMessage.setFromName(fromName);

		sendMail(mailMessage);
	}

	/**
	 * Send email
	 * @param to
	 * @param subject
	 * @param messageText
	 * @param fromEmail
	 * @param fromName
	 * @throws MailServiceException if sending of the mail has failed
	 */
	public void sendMail(String to, String subject, String messageText, String fromEmail, String fromName) {
		final var mailMessage = new MailMessage();

		// Initialize the message object
		mailMessage.getTo().add(to);
		mailMessage.setMessage(messageText);
		mailMessage.setSubject(subject);
		mailMessage.setFromEmail(fromEmail);
		mailMessage.setFromName(fromName);

		sendMail(mailMessage);
	}

	/**
	 * Send email
	 * @param to
	 * @param cc
	 * @param subject
	 * @param messageText
	 * @param fromEmail
	 * @param fromName
	 * @throws MailServiceException if sending of the mail has failed
	 */
	public void sendMail(String to, String cc, String subject, String messageText, String fromEmail, String fromName) {
		final var mailMessage = new MailMessage();

		// Initialize the message object
		mailMessage.getTo().add(to);

		if (cc != null && !cc.isEmpty())
			mailMessage.getCc().add(cc);

		mailMessage.setMessage(messageText);
		mailMessage.setSubject(subject);
		mailMessage.setFromEmail(fromEmail);
		mailMessage.setFromName(fromName);

		sendMail(mailMessage);
	}

	/**
	 * Send email
	 * @param to
	 * @param cc
	 * @param subject
	 * @param messageText
	 * @param fromEmail
	 * @param fromName
	 * @throws MailServiceException if sending of the mail has failed
	 */
	public void sendMail(Collection<String> to, Collection<String> cc, String subject, String messageText, String fromEmail,
			String fromName) {
		final var mailMessage = new MailMessage();

		// Initialize the message object
		mailMessage.setTo(to);
		mailMessage.setCc(cc);
		mailMessage.setMessage(messageText);
		mailMessage.setSubject(subject);
		mailMessage.setFromEmail(fromEmail);
		mailMessage.setFromName(fromName);

		sendMail(mailMessage);
	}

	/**
	 * Send email
	 * @param to
	 * @param cc
	 * @param subject
	 * @param messageText
	 * @param attachmentName
	 * @param attachmentContent
	 * @param fromEmail
	 * @param fromName
	 * @throws MailServiceException if sending of the mail has failed
	 */
	public void sendMail(String to, String cc, String subject, String messageText, String attachmentName, byte[] attachmentContent,
			String fromEmail, String fromName) {
		final var mailMessage = new MailMessage();

		// Initialize the message object
		mailMessage.getTo().add(to);

		if (cc != null && !cc.isEmpty())
			mailMessage.getCc().add(cc);

		mailMessage.setMessage(messageText);
		mailMessage.setSubject(subject);
		mailMessage.setAttachmentName(attachmentName);
		mailMessage.setAttachmentContent(attachmentContent);
		mailMessage.setFromEmail(fromEmail);
		mailMessage.setFromName(fromName);

		sendMail(mailMessage);
	}

}
