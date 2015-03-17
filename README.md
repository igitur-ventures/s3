# Introduction

Provides a `s3` command-line utility that can be used to *put* and *get* files to/from Amazon's S3. This is a very crude and primitive utility, use at your own risks...
This application relies heavily on [Aristid Breitkreuz'](https://github.com/aristidb/aws/) AWS package.

## Get Started

Put your AWS credentials in a file `~/.aws-keys`:

> cat ~/.aws-keys
> default <some AWS Access key> <some private AWS key>

To upload files to a bucket, run

> ./s3 -p mybucket file1 file2 file3

To download files from a bucket, run

> ./s3 -g mybucket file1 file2

